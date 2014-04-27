# Assignment 1

  - Liran Oz
  - Ory Band

## Synopsis

```sh
mvn compile -q pom.xml
./run-local <file> <tasks per worker> <terminate:optional>
```

## Dependencies

  1. Maven
  2. Java 1.6

## Description

We have 3 static classes (+3 `main()`), for local, manager, and worker machines.

### Queues

  1. **localUp**: Notifying manager of new PDF tasks (called *missions*) and asking manager to shut down.
  2. **localDown**: Manager sending results and *shutting-down* notification to locals, so one of them will terminate its instance.
  3. **tasks**: Manager-worker tasks.
  4. **shutdown**: Manager-worker shutdown requests.
  5. **finished**: Worker results.
  6. **closedworkers**: Workers notifying manager of shutdown, so manager could terminate their instance.

### S3 Directory Structure

  1. **inputs**: Local PDF task files.
  2. **results**: Manager PDF task result files.
  3. **files**: Worker PDf task result files.

### Git

Each instances pulls latest code from GitHub, and compiles on runtime.
This of course wouldn't be how we would execute remote processes on production,
but was very convinient for development.
