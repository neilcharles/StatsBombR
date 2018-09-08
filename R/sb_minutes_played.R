sb_minutes_played <- function(sb_clean){
  starting_11s <- sb_clean %>% 
    filter(type.name == "Starting XI") %>% 
    select(match_id, tactics.lineup) %>% 
    unnest() %>% 
    select(-jersey_number, -position.id, -position.name) %>% 
    mutate(start_minute = 0,
           player_id_replaced = NA)
  
  substitutes <- sb_clean %>% 
    filter(type.name == "Substitution") %>% 
    select(match_id, substitution.replacement.id, substitution.replacement.name, player.id, minute) %>% 
    rename(start_minute = minute, player_id_replaced = player.id)
  
  all_players <- starting_11s %>% 
    union_all(substitutes)
  
  last_minutes <- sb_clean %>% 
    group_by(match_id) %>% 
    summarise(full_time = max(minute))
  
  minutes_played <- all_players %>% 
    left_join(all_players, by = c("player.id" = "player_id_replaced", "match_id" = "match_id")) %>% 
    select(match_id, player.name.x, start_minute.x, start_minute.y) %>% 
    rename(start_minute = start_minute.x, end_minute = start_minute.y) %>% 
    left_join(last_minutes, by = "match_id") %>% 
    mutate(end_minute = ifelse(is.na(end_minute), full_time, end_minute)) %>% 
    mutate(minutes_played = end_minute - start_minute)
}