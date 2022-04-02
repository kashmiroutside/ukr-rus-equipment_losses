# Oryx Scrape Code----

#Libraries
library(tidyverse)
library(rvest)
library(tidytext)

#Scrape Commands----

link = "https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html"
page = read_html(link)

name = page %>% html_nodes(".entry-content li") %>% html_text()

equipment_scrape_raw <- name %>% as_tibble()

# Separate equipment from condition
equipment_scrape_sep <- equipment_scrape_raw %>% separate(col = "value", into = c("equipment", "condition"), sep = ":")

# Add country name----
equipment_scrape_sep <- equipment_scrape_sep %>% mutate(country = ifelse(grepl('\\(Unknown\\) vehicle', equipment_scrape_sep$equipment), 'Russia', NA)) %>% 
    fill(country, .direction = "up") %>%
    mutate(country = country %>% replace_na("Ukraine")) %>% 

# Unnest_tokens and clean data----
unnest_tokens(original, condition, token = "regex", pattern = "\\)(?= |\\s|\\r|\\n|\\h)") %>%
    mutate(original = str_remove(string = original, pattern = "\\(")) %>% 
    mutate(original = str_remove(string = original, pattern = "\\)")) %>%  
    mutate(original = str_trim(string = original, side = "both")) %>%
    separate(col = "original", into = c("number", "condition"), sep = '\\s+(?=\\S*$)', remove = FALSE, extra = "merge") %>% 
    mutate(equipment = str_trim(string = equipment, side = "left")) %>%
    separate(col = "equipment", into = c("t_count", "equipment"), sep = " |\\s|\\r|\\n|\\h", remove = FALSE, extra = "merge") %>%
    mutate(equipment = str_trim(string = equipment, side = "both")) %>%
    mutate(number = str_replace(string = number, pattern = " and", replacement = ",")) %>%
    mutate(number = str_remove_all(string = number, pattern = ",")) %>%
    # keep only digits
    mutate(number = str_remove_all(string = number, pattern = "(?<=\\s)[:alpha:]+")) %>%
    # need to regex number so only relevant numbers remain (check 2255)
    mutate(number = str_remove_all(string = number, pattern = "\\'\\w+\\'")) %>% 
    mutate(number = str_remove_all(string = number, pattern = "-\\w+"))
# Some equipment need a special regex
small_patrol_boat_filter <- equipment_scrape_sep %>%  filter(equipment == "Small patrol boats (Sea Guard)") %>% 
    mutate(number = str_remove_all(string = number, pattern = "(?<=\\s).+"))

gaz_66_filter <- equipment_scrape_sep %>% filter(equipment == "GAZ-66") %>% 
    mutate(number = str_remove_all(string = number, pattern = "(?<=\\s)\\w+"))

zhuk_boat_filter <- equipment_scrape_sep %>% filter(equipment == "Zhuk class patrol boat (Sea Guard)") %>% 
    mutate(number = str_remove_all(string = number, pattern = "'\\w+\\s\\w+'"))

equipment_scrape_sep <- equipment_scrape_sep %>% filter(equipment != "Small patrol boats (Sea Guard)") %>% 
    filter(equipment != "GAZ-66") %>% 
    filter(equipment != "Zhuk class patrol boat (Sea Guard)") %>%
    rbind(small_patrol_boat_filter) %>% rbind(gaz_66_filter) %>% rbind(zhuk_boat_filter) %>%
    unnest_tokens(numbers, number)

# Create equipment_type----
equipment_final <-  equipment_scrape_sep %>% 
    mutate(equipment_type = case_when(equipment_scrape_sep$equipment %in% c("T-64BV", "T-72A", "T-72AV", "T-72B", "T-72B Obr. 1989", "T-72B3", "T-72B3 Obr. 2016", "T-80BV", "T-80U", "T-80UK", "T-80UM2", "T-80BVM", "T-90A", "Unknown tank", "T-64B1M", "T-72AMT", "T-64BM 'Bulat'", "T-64BM2 'Bulat'") ~ "Tanks",
                                      equipment_scrape_sep$equipment %in% c("BRM-1K", "BMP-1Khs", "BRDM-2", "MT-LB", "MT-LB with ZU-23 AA gun", "MT-LBM 6MB", "MT-LB Ambulance", "MT-LBu", "1V13 battery fire control center", "1V14 battery command and forward observer vehicle", "1V119 artillery fire direction vehicle", "Vityaz DT-30 articulated tracked carrier", "2S1 with ZU-23 AA gun",  "9P149 Shturm-S ATGM carrier", "BMD-1KSh-A command vehicle", "R-149MA1 command and staff vehicle", "9S470M1 (or variant thereof) command post (for Buk-M1/2)", "TZM-T reloader vehicle (for TOS-1A)", "Unknown BTR-D/BMD-2", "Unknown BTR-80/BTR-82A", "Unknown AFV", "9P148 Konkurs ATGM carrier", "Vepr MRAP", "1V18 'Klyon-1' artillery command and forward observer vehicle", "SNAR-10 battlefield surveillance radar", "R-149MA3 command and staff vehicle", "GAZ-3344-20 'Aleut' articulated tracked carrier", "R-145BM1 command vehicle") ~ "Armoured Fighting Vehicle",
                                      equipment_scrape_sep$equipment %in% c("BMP-1(P)", "BMP-2", "BMP-2K", "BMP-3", "BMD-2", "BMD-4M", "BTR-82A", "Unknown BMP-1/2", "BMP-1TS", "BTR-4", "BTR-3") ~ "Infantry Fighting Vehicle",
                                      equipment_scrape_sep$equipment %in% c("BTR-80", "BTR-D", "BTR-MDM 'Rakushka'", "BTR-60", "BTR-70", "AT105A 'Saxon'", "Unknown BTR") ~ "Armoured Personnel Carriers",
                                      equipment_scrape_sep$equipment %in% c("KamAZ-63968 'Typhoon'", "K-53949 Typhoon-K", "K-53949 'Linza'", "K-53949 'Typhoon-K'") ~ "Mine-Resistant Ambush Protected (MRAP) Vehicles",
                                      equipment_scrape_sep$equipment %in% c("GAZ Tigr", "GAZ Tigr-M", "Iveco LMV 'Rys'", "HMMWV", "KrAZ Cobra", "KrAZ Spartan", "Novator", "Varta", "Kozak-2", "Kozak-2M1", "Triton") ~ "Infantry Mobility Vehicles",
                                      equipment_scrape_sep$equipment %in% c("MP-2IM signals vehicle", "R-166-0.5 signals vehicle", "R-419L1 communications station","P-260-U signal vehicle (for Redut-2US signal and communications system)", "Unknown communications station based on the KamAZ 6x6", "Unknown communications station") ~ "Communications Stations",
                                      equipment_scrape_sep$equipment %in% c("BTR-D APC with UR-67 mine clearing charge", "UR-77 'Meterorit'  mine clearing vehicle", "GMZ-3 minelayer", "BAT-2 heavy engineering vehicle", "BREM-1 armoured recovery vehicle", "BREM-K armoured recovery vehicle", "BREM-Ch ''BREM-4'' armoured recovery vehicle", "REM-KL armoured recovery vehicle", "IMR-2 combat engineering vehicle", "TMM-3 bridge layer", "MTU-72 bridge layer", "PMP floating bridge", "Pontoon bridge", "BMK-130M/BMK-150 towing and motor boat", "PTS-3  tracked amphibious transport", "KamAZ-5350 with EOV-3523 excavator", "KamAZ-5350 with KS-45719-7M crane", "KamAZ-5350 with crane", "Ural-4320 KET-L", "Ural-4320 KT-L", "Ural-4320 with crane", "MDK-3 trench-digging vehicle", "RKhM 'Kashalot'", "BREM-1 ARV", "BSEM-4K", "TMM-3M1 bridge layer", "BMK-150 towing and motor boat", "BTS-4A armoured recovery vehicle", "PP-2005 floating bridge", "KrAZ-255B with excavator") ~ "Engineering Vehicles And Equipment",
                                      equipment_scrape_sep$equipment %in% c("9M111 Fagot", "9M113 Konkurs", "9M113M Konkurs-M", "9M114 Kokon", "RK-3 Corsar", "MBT LAW 'NLAW'", "FGM-148 Javelin") ~ "Anti-Tank Guided Missiles",
                                      equipment_scrape_sep$equipment %in% c("9K38 Igla", "9K333 Verba", "9K310 Igla-1") ~ "Man-Portable Air Defence Systems",
                                      equipment_scrape_sep$equipment %in% c("120mm 2B11/2S12", "120mm 2B11/2S12A") ~ "Heavy Mortars",
                                      equipment_scrape_sep$equipment %in% c("100mm MT-12 anti-tank gun", "120mm 2B16 Nona-K howitzer", "122mm D-30 howitzer 2A18", "152mm 2A65 Msta-B howitzer", "122mm D-30 howitzer", "152mm 2A36 Giatsint-B field-gun", "Unknown towed artillery") ~ "Towed Artillery",
                                      equipment_scrape_sep$equipment %in% c("120mm 2S9 Nona", "120mm 2S23 Nona-SVK", "120mm 2S34 Khosta", "122mm 2S1 Gvozdika", "152mm 2S3 Akatsiya", "152mm 2S19 Msta-S", "152mm 2S33 Msta-SM2", "120mm BTR-3M2", "152mm 2S5 Giatsint-S", "203mm 2S7 Pion", "Unknown SPG") ~ "Self-Propelled Artillery",
                                      equipment_scrape_sep$equipment %in% c("122mm BM-21 'Grad'", "220mm BM-27 'Uragan'", "122mm 2B17 Tornado-G", "220mm TOS-1A") ~ "Multiple Rocket Launchers",
                                      equipment_scrape_sep$equipment %in% c("23mm ZU-23") ~ "Anti-Aircraft Guns",
                                      equipment_scrape_sep$equipment %in% c("BTR-ZD 'Skrezhet'", "2K22M1 Tunguska", "ZSU-23-4 'Shilka'") ~ "Self-Propelled Anti-Aircraft Guns",
                                      equipment_scrape_sep$equipment %in% c("9K33 Osa", "9K35 Strela-10", "9A310M1 TELAR (for Buk-M1-2)", "9A39M1 TEL (for Buk-M1-2)", "9A310M2 TELAR (for Buk-M2)", "9A39M2 TEL (for Buk-M2)", "9A330 Tor TLAR (for 9K330 Tor-M)", "9A331 TLAR (for 9K331 Tor-M1)", "9A331M TLAR (for 9K332 Tor-M2)", "Pantsir-S1", "5P85D (launcher for S-300PS)", "5P85S (launcher for S-300PS)", "Buk-M1", "9A310M1-2 TELAR (for Buk-M1-2)", "9A39M1-2 TEL (for Buk-M1-2)", "9A317 TELAR (for Buk-M2)", "9A316 TEL (for Buk-M2)", "Unknown Buk SAM system", "9A330 Tor TLAR (for 9K330 Tor)", "5P851A  (launcher for S-300PT)", "9A310M1 TELAR (for Buk-M1)") ~ "Surface-To-Air Missile Systems",
                                      equipment_scrape_sep$equipment %in% c("9S18М1(-3) (for Buk-M3)", "Fara ground surveillance radar", "1L261 (for 1L260 Zoopark-1M counter-battery radar complex)", "P-19 'Flat Face B'", "P-35/37 'Bar Lock'", "PRV-13 'Odd Pair'", "1L22 'Parol'", "30N6 'Flap Lid'", "36D6 'Tin Shield'", "AN/TPQ-36 lightweight counter mortar radar", "AN/TPQ-49 lightweight counter mortar radar", "PPRU-1(M) '9S80(-1)' 'Sborka' (for 9K35 Strela-10)", "30N6 'Flap Lid' (for S-300PS)", "5N63-1 'Flap Lid' on 40V6M mast (for S-300PT)", "5N66M 'Clam Shell' on 40V6M mast (for S-300PT)") ~ "Radars",
                                      equipment_scrape_sep$equipment %in% c("R-330BMV Borisoglebsk-2B", "R-330ZH Zhitel",  "Torn(-MDM)", "1RL257 Krasukha-4 command post") ~ "Jammers And Deception Systems",
                                      equipment_scrape_sep$equipment %in% c("Su-25 close air support aircraft", "Su-30SM multirole aircraft", "Su-34 strike aircraft", "An-26 transport aircraft", "MiG-29 fighter aircraft", "Su-25 attack aircraft", "Su-27 fighter aircraft") ~ "Aircraft",
                                      equipment_scrape_sep$equipment %in% c("Mi-8 transport helicopter", "Mi-24V/P attack helicopter", "Mi-35M attack helicopter", "Unknown Mi-24/35 attack helicopter", "Mi-28 attack helicopter", "Ka-52 'Alligator' attack helicopter", "Unknown helicopter", "Mi-24 attack helicopter") ~ "Helicopters",
                                      equipment_scrape_sep$equipment %in% c("Forpost reconnaissance UAV", "Orlan-10 reconnaissance UAV", "Eleron-3 reconnaissance UAV", "ZALA Aero KUB-BLA loitering munition", "E95M target drone (likely used as unmanned bait in order for Ukraine to reveal the location of air defence systems)", "Tu-141 reconnaissance UAV", "A1-SM Fury reconnaissance UAV", "UJ-22 reconnaissance UAV", "Leleka-100 reconnaissance UAV", "Bayraktar TB2 UCAV", "Weaponised VTOL UAV") ~ "Unmanned Aerial Vehicles",
                                      equipment_scrape_sep$equipment %in% c("Project 03160 Raptor class patrol boat", "Project 1171 Tapir class landing ship 'Saratov (BDK-65)'", "Project 775 Ropucha class landing ship", "Krivak III class frigate 'Hetman Sahaydachniy'", "Gyurza-M class gunboat", "Island class patrol boat P190 Slovyansk", "Zhuk class patrol boat (Sea Guard)", "Small patrol boats (Sea Guard)", "Sapphire search and rescue vessel (Operated by the Maritime Search and Rescue Service of Ukraine)") ~ "Naval Ships",
                                      equipment_scrape_sep$equipment %in% c("Fuel train") ~ "Logistics Trains",
                                      equipment_scrape_sep$equipment %in% c("GAZ-66", "KrAZ-22B", "ZiL-131", "9T452 transporter-loader (for BM-27 'Uragan' MRL)", "9T217 transloader (for 9K33 Osa)", "GAZ-3308", "GAZ Sobol", "Ural-375D", "Ural-4320", "Ural-43206", "Ural Federal", "Ural-63704-0010 Tornado-U", "Ural-542301 tank transporter", "KamAZ 4x4", "KamAZ 6x6", "KamAZ 8x8", "KamAZ Avtozaks", "KamAZ with armoured cabin", "KamAZ-6350 8x8 artillery tractor", "Civilian KamAZ 6x6 converted for military use", "UAZ-469", "UAZ-452", "UAZ Patriot", "UAZ-23632", "Armoured SUV", "Unknown truck", "(Unknown) vehicle", "KrAZ-255B", "KrAZ-260", "KrAZ-6322", "KrAZ-5233", "ZiL-130", "ZiL-157", "KamAZ", "MAZ-537", "MAZ", "Unknown vehicle") ~ "Trucks Vehicles and Jeeps",
                                      TRUE ~ 'New Equipment')) %>% 
    #special indexing for issues like Cyrillic unicode
    mutate(equipment_type = case_when(
        str_detect(equipment_scrape_sep$equipment, "Sorum class seagoing tugboat") ~ "Naval Ships",
        str_detect(equipment_scrape_sep$equipment, "Barnaul-T") ~ "Communications Stations",
        str_detect(equipment_scrape_sep$equipment, "Metis-M|Kornet") ~ "Anti-Tank Guided Missiles",
        str_detect(equipment_scrape_sep$equipment, "\\(-3\\) \\(for Buk-M3|Tall King") ~ "Radars",
        
        TRUE ~ equipment_type))

# Optional changes for special condition which escaped my regex formulas
equipment_final <- equipment_final %>%  
    mutate(condition = case_when(condition %in% c("tb2", "ground", "artillery", "stripped", "forpost-r", "orion", "russia", "munition", "sunk") ~ "destroyed",
                                 TRUE ~ condition))

# Check for newly added equipment not yet indexed      
equipment_final %>% filter(equipment_type == "New Equipment") %>% view()