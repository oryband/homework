# Data types

## Experiments

Experiment \[Passive\] | Runnable Experiment \[Observable\] \[Active\] |
---------------------- | --------------------------------------------- |
id                     | 1 program hour = 100 real-life ms             |
specialization         |                                               |
run time               |                                               |
pre-requirements       |                                               |
equipment              |                                               |
reward                 |
status                 |

## Items

Repository \[Passive\]                          |
----------------------                          |
item list: [(name, amount), (name, amount), ..] |


ScienceStore \[Passive\]    | EquipmentPackage | Laboratory                 | Scientist      |
------------------------    | ---------------- | ----------                 | ---------      |
equipment packages          | name             | name of head of laboratory | name           |
scientists                  | # of items       | specialization             | specialization |
laboratories                | total cost       | number of scientists       | cost           |

## Laboratories and scientists

HeadOfLaboratory \[Passive\]                             |
----------------------------                             |
name                                                     |
specialization                                           |
number of scientists (threads)                           |
thread pool - 1 thread for each scientist.               |
**NOTE** all the first 3 fields are also in `Laboratory` |


ChiefScientist \[Observer\] \[Passive\] | ChiefScientistAssistant \[Active\] |
--------------------------------------- | ---------------------------------- |
head-of-laboratory list                 |                                    |
experiment list                         |                                    |
statistcs                               |                                    |
science store                           |                                    |
repository                              |                                    |
chief scientist assistant               |                                    |


# Statistics

Statistics \[Passive\]    |
----------------------    |
budget                    |
money gained              |
money spent               |
scientists purchased      |
equipment packs purhcased |
laboratories purchased    |
completed experiments     |
