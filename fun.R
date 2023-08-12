#fix with stringi::stri_reverse
library(stringi)
palindrome <- function(name) {
  
  name <- str_to_upper(name)
  
  #odd number of letters
  if(str_length(name) %% 2 == 1) {
    
    med_position <- floor(str_length(name) / 2 + 1)
    i <- 1
    
    while(i < med_position) {
      
      if(str_sub(name, i, i) != str_sub(name, str_length(name) - i + 1, str_length(name) - i + 1)) 
            {
              pal <- FALSE
              break
            } else {
              pal <- TRUE
            }
        i <- i + 1
      }
    
    
    
  } else {
    
    med_position <- str_length(name) / 2 + 1
    i <- 1
    
    while(i < med_position) {
      
      if(str_sub(name, i, i) != 
         str_sub(name, str_length(name) - i + 1, str_length(name) - i + 1)) 
      {
        pal <- FALSE
        break
      } else {
        pal <- TRUE
      }
      i <- i + 1
    }
    
  }
  
  return(pal)

}

  
people_pal <- people_raw %>% 
  rowwise() %>% 
  mutate(pal = str_to_upper(nameLast) == stri_reverse(str_to_upper(nameLast))) %>%  
  filter(pal == TRUE)
