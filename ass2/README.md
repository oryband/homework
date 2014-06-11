# Assignment 2

  - Liran Oz
  - Ory Band

## Synopsis

```sh
$ mvn compile -q pom.xml
$ ./run-class.sh <file> <Top PMI Pairs> <Threshold>
```

## Dependencies

  1. Maven
  2. Java 1.7


## Steps

### Step 1: Count

  1. Recieves 5-grams.
  2. Mappers emits, where `w` is a 5-gram word:
    1. `{ decade, w, wi : c(w,wi) }`
    2. `{ decade, w, wi : c(wi,w) }`, i=1..4 (its neighbours)

  3. Reducer writes:
    1. `{ decade, w, * : c(w) }` if key is `decade, w, *`
    2. `{ <w,wi> : c(w), c(w,wi) }` if key is `<w,wi>`
  4. In addition, driver uploads:
    1. Map output records.
    2. Number of words in corpus, per decade.

### Step 2: Join

  1. Mappers emits: `{ <w1,w2> : decade, w1, c(w1), c(w1,w2) }`
  2. Reducer writes: `{ <decade, w1 ,w2> : c(w1), c(w2), c(w1,w2) }`
  3. In addition, driver uploads map output records.

### Step 3: Calculate

  1. Mappers emits: `{ <w1,w2> : decade, w1, c(w1), c(w1,w2) }`
  2. Reducer writes: `{ decade, pmi : w1, w2 }`
  3. In addition, driver downloads previous steps' map output records,
     sums them with its local records, and uploads result to S3.

### Step 4: Last Decade

  1. Mappers emits: `{ <w1,w2> : decade, w1, c(w1), c(w1,w2) }`
  2. Reducer writes: `{ lastDecade, pmi : w1, w2 }`

### Step 5: F-Measure

  1. Mappers emits: `{ tp/fp/tn/fn : 1 }`
  2. Reducer sums (doesn't write): `{ tp/fp/tn/fn : Number of tp/fp/tn/fn test results }`
  3. Afterwards, driver uploads calculation input and results to S3.

## S3 Directory Structure

  1. **steps/**: Job flow steps input and output.
  2. jars/
  3. logs/
  4. **lucene/**: Text search engine, used for filtering stop words and other special characters in corpus.
