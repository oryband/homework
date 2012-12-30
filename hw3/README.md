# Action items

1. Use `final` wherever possible:
    1. Fields, functions, and *classes*.
    2. Make sure mutable fields are private, and are **never returned** (no getters).
    3. We can also return new instances of mutable objects, instead of our own - so we won't accidently change stuff.
    4. **Note** an object actually becomes immutable - `final` *after* construction.
2. **TODO:** Check if we should use `java.util.Collections.unmodifiableMap(Map)`.
3. Who should be:
    1. `runnable`? Only `RunnableExperiment` and `ChiefScientistAssistant`? Anything else perhaps?
    2. Thread-confined?
    3. Shared read-only?
    4. Shared thread-safe?
4. Add @INV, @PRE, @POST and whatever else to all fields & functions.
5. Do tests for everything? God please no.

# Data types

## Experiments

Experiment \[Passive\] | Runnable Experiment \[Observable\] \[Active\] |
---------------------- | --------------------------------------------- |
id                     | 1 program hour = 100 real-life ms             |
specialization         |                                               |
run time               |                                               |
pre-requirements       |                                               |
equipment              |                                               |
reward                 |                                               |
status                 |                                               |

## Items

Repository \[Passive\]                          |
----------------------                          |
item list: [(name, amount), (name, amount), ..] |


ScienceStore \[Passive\] | EquipmentPackage | Laboratory                 | Scientist      |
------------------------ | ---------------- | ----------                 | ---------      |
equipment packages       | name             | name of head of laboratory | name           |
scientists               | # of items       | specialization             | specialization |
laboratories             | total cost       | number of scientists       | cost           |

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

## Statistics

Statistics \[Passive\]    |
----------------------    |
budget                    |
money gained              |
money spent               |
scientists purchased      |
equipment packs purhcased |
laboratories purchased    |
