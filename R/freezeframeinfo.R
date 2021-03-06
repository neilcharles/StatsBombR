freezeframeinfo <- function(dataframe){
  Shots.FF <- dataframe %>%
    filter(type.name == "Shot") %>%
    dplyr::select(id, shot.freeze_frame, location.x, location.y, DistToGoal, AngleToGoal)
  Shots.FF <- as_tibble(Shots.FF)
  Shots.FF <- Shots.FF %>%
    mutate(Angle.New = ifelse(AngleToGoal > 90, 180 - AngleToGoal, AngleToGoal)) %>%
    mutate(Angle.Rad = Angle.New*pi/180) %>%
    mutate(Dist.x = 120 - location.x,
           Dist.y = ifelse(location.y > 40, location.y - 40, 40 -location.y)) %>%
    mutate(new.Dx = (sin(Angle.Rad)*(DistToGoal+1)),
           new.Dy = (cos(Angle.Rad)*(DistToGoal+1)) ) %>%
    mutate(new.x =  120 - new.Dx,
           new.y =  ifelse(location.y < 40, 40 - new.Dy,
                           new.Dy + 40))

  ##Trying a different method for the speed.
  myList <- Shots.FF$shot.freeze_frame
  fixnull <- function(x) {
    if(is.data.frame(x)){
      return(x)
    } else {
      return(setNames(data.frame(matrix(ncol = ncol(myList[[1]]), nrow = 1)), names(myList[[1]])))
    }
  }

  # Apply the written function above to every element in myList
  myList <- lapply(myList, fixnull)

  # "bind_rows" with mynewList
  df <- bind_rows(myList, .id = "id")

  ##Index Length
  idtable <- df %>%
    mutate(id = as.numeric(id)) %>%
    group_by(id) %>%
    slice(1) %>%
    select(id) %>%
    ungroup() %>%
    mutate(sbid = Shots.FF$id,
           x =Shots.FF$location.x,
           y = Shots.FF$location.y,
           new.x = Shots.FF$new.x,
           new.y = Shots.FF$new.y) %>%
    mutate(id = as.character(id))

  #Join with the freeze frame table
  df <- left_join(df, idtable)
  df <- df %>% select(-id) %>% rename(id = sbid) %>% select(id, everything())

  ##calculate the new information
  df <- df %>%
    mutate(location.x = str_extract(location, "[:digit:]+"),
           location.y = str_extract(location, "[:blank:][:digit:]+")) %>%
    mutate(location.x = as.numeric(location.x),
           location.y = as.numeric(location.y)) %>%
    mutate(distance = sqrt((x - location.x)^2 + (y - location.y)^2)) %>%
    mutate(distance = ifelse(distance== 0, 1/3, distance))

  df <- df %>%
    rowwise() %>%
    mutate(InCone = sp::point.in.polygon(location.x,
                                         location.y,
                                         c(120, 120, new.x),
                                         c(35, 45, new.y)))

  df <- df %>%
    ungroup() %>%
    mutate(InCone = ifelse(InCone > 0, 1, InCone))

  ##Summarise values down for each id
  density <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == FALSE) %>%
    summarise(density = sum(1/distance))

  density.incone <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == FALSE & InCone == 1) %>%
    summarise(density.incone = sum(1/distance))

  density.A <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == TRUE) %>%
    summarise(density.A = sum(1/distance))

  density.incone.A <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == TRUE & InCone == 1) %>%
    summarise(density.incone.A = sum(1/distance))

  distance.ToD1 <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == FALSE) %>%
    arrange(distance) %>%
    slice(1) %>%
    select(id, distance.ToD1 = distance)

  distance.ToD2 <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == FALSE) %>%
    arrange(distance) %>%
    slice(2) %>%
    select(id, distance.ToD2 = distance)

  AttackersBehindBall <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == TRUE) %>%
    summarise(AttackersBehindBall = n())

  DefendersBehindBall <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == FALSE) %>%
    summarise(DefendersBehindBall = n())

  AttackersInCone <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == TRUE & InCone == 1) %>%
    summarise(AttackersInCone = n())
  DefendersInCone <- df %>%
    group_by(id) %>%
    filter(location.x >= x & teammate == FALSE & InCone == 1) %>%
    summarise(DefendersInCone = n())

  #We need to join these
  Shots.FF <- Shots.FF %>%
    left_join(density) %>%
    left_join(density.incone) %>%
    left_join(distance.ToD1) %>%
    left_join(distance.ToD2) %>%
    left_join(density.A) %>%
    left_join(density.incone.A) %>%
    left_join(AttackersBehindBall) %>%
    left_join(DefendersBehindBall) %>%
    left_join(AttackersInCone) %>%
    left_join(DefendersInCone)

  #We need some way of changing the value if everything gets filtered out.
  ##These are easy.
  Shots.FF <- Shots.FF %>%
    mutate(density = ifelse(is.na(density), 0, density),
           density.incone = ifelse(is.na(density.incone), 0, density.incone),
           density.A = ifelse(is.na(density.A), 0, density.A),
           density.incone.A = ifelse(is.na(density.incone.A), 0, density.incone.A),
           AttackersBehindBall = ifelse(is.na(AttackersBehindBall), 0, AttackersBehindBall),
           DefendersBehindBall = ifelse(is.na(DefendersBehindBall), 0, DefendersBehindBall),
           AttackersInCone = ifelse(is.na(AttackersBehindBall), 0, AttackersBehindBall),
           DefendersInCone = ifelse(is.na(DefendersBehindBall), 0, DefendersBehindBall))

  ##The only ones we need to change are the distance metrics.
  posdist1 <- df %>%
    group_by(id) %>%
    filter(location.x < x & teammate == FALSE) %>%
    arrange(distance) %>%
    slice(1) %>%
    select(id, posdist1 = distance)

  posdist2 <- df %>%
    group_by(id) %>%
    filter(location.x < x & teammate == FALSE) %>%
    arrange(distance) %>%
    slice(2) %>%
    select(id, posdist2 = distance)

  Shots.FF <- Shots.FF %>%
    left_join(posdist1) %>%
    left_join(posdist2)

  Shots.FF <- Shots.FF %>%
    mutate(distance.ToD1 = ifelse(is.na(distance.ToD1) & is.na(posdist1), NA,
                                  ifelse(is.na(distance.ToD1), posdist1, distance.ToD1)),
           distance.ToD2 = ifelse(is.na(distance.ToD2) & is.na(posdist2), NA,
                                  ifelse(is.na(distance.ToD2), posdist2, distance.ToD2)))


  Shots.FF <- Shots.FF %>% dplyr::select(id, density, density.incone, distance.ToD1, distance.ToD2,
                                         density.A, density.incone.A, AttackersBehindBall, DefendersBehindBall,
                                         AttackersInCone, DefendersInCone)

  dataframe <- left_join(dataframe, Shots.FF)


  return(dataframe)

}
