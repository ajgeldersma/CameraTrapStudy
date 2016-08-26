# Import and format all the files from winter 2014-15
# Anna Moeller
# 7/21/2015

# I used 8 different formats. ICK! (Notes in thesis notes notebook)
# 1: S AM11, S AM12, S AM13, S AM14, S AM16, IG AM21, IG AM22
# 2: S AM17, S AM18, S AM19, S AM20
# 3. IG AM23, IG AM28, IG AM33, IG AM38, IG AM39, IG AM40, IG AM47, DC AM41
# 4. DC AM42, DC AM43
# 5. DC AM45
# 6. DC AM49, DC AM51, DC AM53, DC AM57, BS AM46, BS AM48, BS AM52, BS AM54, BS AM55, BS AM56
# 7. BS AM58, BS AM59
# 8. BS AM61, all of Buckhorn, all of Kenney Creek

# Import Boulder Spring
bs.fn <- function(){
  # For the first 6 cameras, I used the first version of Timelapse.
  # For the next 3 cameras, I used the second version of Timelapse, my first iteration
  
  # Initialize region and site
  region <- "Beaverhead Reg. 6" 
  site <-"BS" 
  
  # list files
  files <- grep(site, list.files(region), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Import image data/combine metadata and timelapse.R")
  f1 <- do.call(rbind, lapply(files[1:6], combo_timelapse1_fn, region = region, site = site))
  f78 <- do.call(rbind, lapply(files[7:8], combo_timelapse2_fn, region = region, site = site))
  f9 <- combo_timelapse2_fn(region, site, cam = files[9])
  
  # Make them all the same. Fill in missing columns with NAs
  ### (because I added columns as I went along, in newer template versions)
  f1 <- mutate(f1, bird = NA)
  f78 <- mutate(f78, review = NA)
  f789 <- bind_rows(f78, f9)
  f2 <- mutate(f789, op.state = opstate, collar.yn = collaryn, animal.other = animalother, 
               animal.unkn = animalunkn, Image.Quality = ImageQuality) %>%
    select(-MarkForDeletion, -opstate, -collaryn, -animalother, -animalunkn, -ImageQuality)
  bs <- bind_rows(f1, f2)
  
  # Need to fix capitalization within the columns
  bs <- mutate(bs,
               present = tolower(present),
               op.state = tolower(op.state),
               background = tolower(background),
               site = "BS")
  
  return(bs)
}

################################################################################
# Import Deer Canyon
dc.fn <- function(){
  
  # Initialize region and site
  region <- "Beaverhead Reg. 6" 
  site <-"DC" 
  
  # List files
  files <- grep(site, list.files(region), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Import image data/combine metadata and timelapse.R")
  
  # For Beaverhead Reg. 6/Deer Canyon
  f1 <- combo_timelapse1_fn(region, site, cam = "DC AM41")
  f23 <- do.call(rbind, lapply(files[2:3], combo_timelapse1_fn, region = region, site = site))
  f4 <- combo_timelapse1_fn(region, site, cam = "DC AM45")
  f5678 <- do.call(rbind, lapply(files[5:8], combo_timelapse1_fn, region = region, site = site))
  
  # Make them all the same.
  f1 <- mutate(f1, present = elk.present) %>%
    select(-elk.present)
  f23 <- mutate(f23, bird = NA)
  f123 <- bind_rows(f1, f23) %>%
    mutate(background = NA,
           uncounted = NA)
  f1234 <- bind_rows(f123, f4) %>%
    mutate(sheep = NA,
           cattle = NA,
           horse = NA,
           uncounted = NA)
  dc <- bind_rows(f1234, f5678)
  
  # Fix capitalization within the columns
  dc <- mutate(dc,
               present = tolower(present),
               op.state = tolower(op.state),
               background = tolower(background),
               site = "DC")
  dc$present[dc$present == "" & dc$op.state == "setup"] <- "humans present"
  
  return(dc)
}

################################################################################
# import Italian Gulch
ig.fn <- function(){
  # Initialize region and site
  region <- "Panhandle" 
  site <- "IG"
  
  # List camera names
  cam <- grep(paste(site, "AM", sep = " "), list.files(region), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Import image data/combine metadata and timelapse.R")
  
  # Two different formats
  f12 <- do.call(rbind, lapply(cam[1:2], combo_timelapse1_fn, region = region, site = site))
  f39 <- do.call(rbind, lapply(cam[3:9], combo_timelapse1_fn, region = region, site = site))
  
  # Bind them together
  f12$present <- NA
  f12$present[f12$elk.present != "" & f12$animals.present == ""] <- "elk present"
  f12$present[f12$animals.present != "" & f12$elk.present == ""] <- "other animals present"
  f12$present[f12$animals.present != "" & f12$elk.present != ""] <- "elk and other animals present"
  f12$present[f12$elk.present == "" & f12$animals.present == ""] <- "no animals present"
  f12$present[f12$op.state == "Setup"] <- "humans present"
  
  f1 <- mutate(f12, 
               op.state = tolower(op.state),
               cattle1 = substr(cattle.tag, 1, 1),
               cattle2 = substr(cattle.tag, 2, 2)) %>%
    select(-animals.present, -elk.present)
  f1$cattle1[f1$cattle1 == "?"] <- "unknown"
  f1$cattle2[f1$cattle2 == "?"] <- "unknown"
  f2 <- mutate(f39, 
               present = tolower(elk.present),
               site = "IG") %>%
    select(-elk.present)
  out <- bind_rows(f1, f2)
}

################################################################################
# Import Swinnerton
s.fn <- function(){
  # Initialize region and site
  region <- "Panhandle" 
  site <-"S" 
  
  # List camera names
  cam <- grep(paste(site, "AM", sep = " "), list.files(region), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Import image data/combine metadata and timelapse.R")
  out <- do.call(rbind, lapply(cam, combo_timelapse1_fn, region = region, site = site)) 
  
  # Fix the present column
  out$present <- NA
  out$present[out$elk.present != "" & out$animals.present == ""] <- "elk present"
  out$present[out$animals.present != "" & out$elk.present == ""] <- "other animals present"
  out$present[out$animals.present != "" & out$elk.present != ""] <- "elk and other animals present"
  out$present[out$elk.present == "" & out$animals.present == ""] <- "no animals present"
  out$present[out$op.state == "Setup"] <- "humans present"
  
  out$cattle1 <- substr(out$cattle.tag, 1, 1)
  out$cattle2 <- substr(out$cattle.tag, 2, 2)
  out$cattle1[out$cattle1 == "?"] <- "unknown"
  out$cattle2[out$cattle2 == "?"] <- "unknown"
  out$cattle1[out$cattle.tag == "can't see"] <- "unknown"
  out$cattle2[out$cattle.tag == "can't see"] <- "unknown"
  
  out2 <- mutate(out, 
               op.state = tolower(op.state),
               site = "S") %>%
    select(-animals.present, -elk.present, -cattle.tag)
}

################################################################################
# Import Buckhorn
bh.fn <- function(){
  
  # Initialize region and site
  region <- "Beaverhead Reg. 7" 
  site <-"BH" 
  
  # List camera names
  cam <- grep(paste(site, "AM", sep = " "), list.files(region), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Import image data/combine metadata and timelapse.R")
  out <- do.call(rbind, lapply(cam, combo_timelapse2_fn, region = region, site = site))
  
  # Add site
  out <- mutate(out, site = "BH")
}

# Import Kenney Creek
kc.fn <- function(){
  
  # Initialize region and site
  region <- "Beaverhead Reg. 7" 
  site <-"KC" 
 
  # List camera names
  cam <- grep(paste(site, "AM", sep = " "), list.files(region), value = T)
  
  # Call combo_fn, inputs: region, site, cam
  source("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Image Analysis/Import image data/combine metadata and timelapse.R")
  out <- do.call(rbind, lapply(cam, combo_timelapse2_fn, region = region, site = site))
  
  # Add site
  out <- mutate(out, site = "KC")
}
  