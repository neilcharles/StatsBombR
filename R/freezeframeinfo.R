freezeframeinfo <- function(dataframe){
  Shots.FF <- dataframe %>%
    filter(type.name == "Shot") %>%
    dplyr::select(shot.freeze_frame, location.x, location.y, DistToGoal, AngleToGoal)

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

  cleanFF <- function(ff, x, y, new.x, new.y){

    if(is.null(dim(ff))){
      return(cbind(density = NA,
                   density.incone = NA,
                   distance.ToD1 = NA,
                   distance.ToD2 = NA,
                   density.A = NA,
                   density.incone.A = NA,
                   AttackersBehindBall = NA,
                   DefendersBehindBall = NA))
    } else

      ff.df <- ff %>%
        mutate(location.x = str_extract(location, "[:digit:]+"),
               location.y = str_extract(location, "[:blank:][:digit:]+")) %>%
        mutate(location.x = as.numeric(location.x),
               location.y = as.numeric(location.y)) %>%
        mutate(distance = sqrt((x - location.x)^2 +
                                 (y - location.y)^2)) %>%
        mutate(distance = ifelse(distance== 0, 1/3, distance))

    ##Create the cone
    Cone.df <- rbind(c(120, 36 - 1),
                     c(120, 44 + 1),
                     c(new.x, new.y))

    ff.df$InCone <- pnt.in.poly(cbind(ff.df$location.x, ff.df$location.y), Cone.df)$pip

    ff.a <- ff.df %>%
      filter(teammate == "TRUE")
    ff.a <- ff.a %>%
      filter(location.x >= x)

    AttackersBehindBall <- as.numeric(dim(ff.a)[1])

    ff.df <- ff.df %>%
      filter(teammate == "FALSE" & position.name != "Goalkeeper")

    ff.df <- ff.df %>%
      filter(location.x >= x)

    DefendersBehindBall <- as.numeric(dim(ff.df)[1])


    if(dim(ff.df)[1] == 0){ ###Defending Information
      return(cbind(density = NA,
                   density.incone = NA,
                   distance.ToD1 = NA,
                   distance.ToD2 = NA))
    } else {


      ff.dfD <- ff.df %>%
        filter(location.x >= x) %>%
        summarise(Density = sum(1/distance))

      if(dim(ff.dfD)[1] == 0){
        density = 0
      } else {

        density = ff.dfD$Density
      }

      ff.dfIn <- ff.df %>%
        filter(InCone == 1) %>%
        summarise(Density = sum(1/distance))

      if(dim(ff.dfIn)[1] == 0){
        density.incone = 0
      } else {
        density.incone = ff.dfIn$Density
      }

      Dist1 <- ff.df %>%
        filter(location.x >= x) %>%
        arrange((distance)) %>%
        slice(1) %>%
        select(distance)

      if(dim(Dist1)[1] == 0){
        Dist1 <- ff.df %>%
          filter(location.x < x) %>%
          arrange((distance)) %>%
          slice(1) %>%
          select(distance)
        if(dim(Dist1)[1] == 0){
          distance.ToD1 = Inf
        } else{
          distance.ToD1 = -Dist1$distance
        }
      } else {
        distance.ToD1 = Dist1$distance
      }

      Dist2 <- ff.df %>%
        filter(location.x >= x) %>%
        arrange((distance)) %>%
        slice(2) %>%
        select(distance)

      if(dim(Dist2)[1] == 0){
        Dist2 <- ff.df %>%
          filter(location.x < x) %>%
          arrange((distance)) %>%
          slice(2) %>%
          select(distance)
        if(dim(Dist2)[1] == 0){
          distance.ToD2 = Inf
        } else{
          distance.ToD2 = -Dist2$distance
        }
      } else {
        distance.ToD2 = Dist2$distance
      }


    } ###End Defensive Information

    if(dim(ff.a)[1] == 0){ ###Attacking Information
        density.A = NA
        density.incone.A = NA
    } else {
      ff.Adf <- ff.a %>%
        filter(location.x >= x) %>%
        summarise(Density = sum(1/distance))

      if(dim(ff.Adf)[1] == 0){
        density.A = 0
      } else {
        density.A = ff.Adf$Density
      }

      ff.AdfIn <- ff.a %>%
        filter(InCone == 1) %>%
        summarise(Density = sum(1/distance))

      if(dim(ff.AdfIn)[1] == 0){
        density.incone.A = 0
      } else {
        density.incone.A = ff.AdfIn$Density
      }

    } ###End Attacking Information

    return(cbind(density = density,
                 density.incone = density.incone,
                 distance.ToD1 = distance.ToD1,
                 distance.ToD2 = distance.ToD2,
                 density.A = density.A,
                 density.incone.A = density.incone.A,
                 AttackersBehindBall = AttackersBehindBall,
                 DefendersBehindBall = DefendersBehindBall))
  } ##End cleanFF function

  Shots.FF <- Shots.FF %>%
    mutate(FFinfo = pmap(list(shot.freeze_frame, location.x, location.y, new.x, new.y), cleanFF))

  densityfunct <- function(element){
    return(element[1,1])
  }
  densityinfunct <- function(element){
    return(element[1,2])
  }
  distance1funct <- function(element){
    return(element[1,3])
  }
  distance2funct <- function(element){
    return(element[1,4])
  }
  densityAfunct <- function(element){
    return(element[1,5])
  }
  densityAinfunct <- function(element){
    return(element[1,6])
  }
  Abehindfunct <- function(element){
    return(element[1,7])
  }
  Dbehindfunct <- function(element){
    return(element[1,8])
  }

  Shots.FF$density <- as.numeric(lapply(Shots.FFout, densityfunct))
  Shots.FF$density.incone <- as.numeric(lapply(Shots.FFout, densityinfunct))
  Shots.FF$distance.ToD1 <- as.numeric(lapply(Shots.FFout, distance1funct))
  Shots.FF$distance.ToD2 <- as.numeric(lapply(Shots.FFout, distance2funct))
  Shots.FF$density.A <- as.numeric(lapply(Shots.FFout, densityAfunct))
  Shots.FF$density.incone.A <- as.numeric(lapply(Shots.FFout, densityAinfunct))
  Shots.FF$AttackersBehindBall <- as.numeric(lapply(Shots.FFout, Abehindfunct))
  Shots.FF$DefendersBehindBall <- as.numeric(lapply(Shots.FFout, Dbehindfunct))

  Shots.FF <- Shots.FF %>%
    mutate(density = as.numeric(map(.$FFinfo, 1)),
           density.incone = as.numeric(map(.$FFinfo, 2)),
           distance.ToD1 = as.numeric(map(.$FFinfo, 3)),
           distance.ToD2 = as.numeric(map(.$FFinfo, 4)),
           density.A = (map(.$FFinfo, 5)),
           density.incone.A = (map(.$FFinfo, 6)),
           AttackersBehindBall = (map(.$FFinfo, 7)),
           DefendersBehindBall = (map(.$FFinfo, 8)))


  Shots.FF <- Shots.FF %>% dplyr::select(id, density, density.incone, distance.ToD1, distance.ToD2,
                                         density.A, density.incone.A, AttackersBehindBall, DefendersBehindBall)

  dataframe <- left_join(dataframe, Shots.FF)
  return(dataframe)

} ##End Freeze Frame Info Function