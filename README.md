# ukr-rus-equipment_losses
## Who
[Oryx](https://www.oryxspioenkop.com/) is a team of OSINT collaborators who have meticulously tracked Ukrainian and Russian military equipment losses since the Russian invasion began on February 24th, 2022.
## What
*This R script allows you to scrape their public [website](https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html) and creates a dataset for analysis on equipment losses from the Russian invasion of Ukraine*
## Why
When Russian forces first invaded Ukraine, I was devastated. After the initial shock wore off I became obsessed with trying to help Ukraine. I researched organizations and donated as much as I could, but I wanted to do more. When I discovered Oryx, I decided my limited skills in data analysis and visualization might be put to good use.
## Data Dictionary (equipment_final)
| Variable | Description                                    |
 ----------|------------------------------------------------
| country  | Country                                        |
| t_count  | How many of pieces of the equipment are listed |
| equipment| Name of equipment                              |
| original | This is what the original description read     |
| condition| Assessed condition of equipment                |
| numbers  | Identification number within equipment listing |
| equipment_type| Type of equipment (i.e. T-64BV = Tanks). As new equipment is added this may need to be updated|
