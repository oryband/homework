package com.dsp.ass3;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Counters;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.jgrapht.Graphs;
import org.jgrapht.alg.DijkstraShortestPath;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;

import com.google.common.collect.Lists;


public class Biarcs {
    private static boolean inverse;
    private static final int maximumPathLength = 4;

    private static final String
        inverseArg = "inverse",
        tokenDelim = " ",
        splitTokenDelim = "/",
        posLabelNounPrefix = "NN",
        posLabelVerbPrefix = "VB",
        posLabelPlural = "S",
        inversePrefix = "@",
        depTreeDelim = ":",
        depTreeTokenDelim = ",",
        wordDelimStart =  "<",
        wordDelimEnd =  ">";

    // Ngram reader.
    public static class MySequenceFileInputFormat extends SequenceFileInputFormat<LongWritable, Text> {}

    private static final Logger logger = Utils.setLogger(Logger.getLogger(Biarcs.class.getName()));


    // Write { N1, N2 : dep-tree, total-count }
    public static class MapClass extends Mapper<LongWritable, Text, Text, Text> {
        private Stemmer stemmer = new Stemmer();

        private Text newKey = new Text(),
                newValue = new Text();


        // Stem a word i.e. "Catlike" --> "Cat"
        public String stemWord(String word) {
            stemmer.add(word.toCharArray(), word.length());
            stemmer.stem();
            return new String(stemmer.getResultBuffer()).substring(0, stemmer.getResultLength());
        }


        // 'word/pos-label/dep-label/head-index' parsing methods.
        public String getWordFromToken(String token) {
            return token.substring(0, token.indexOf(splitTokenDelim));
        }

        public String getPosLabelFromToken(String token) {
            return token.split(splitTokenDelim)[1];
        }

        public String getDepLabelFromToken(String token) {
            return token.split(splitTokenDelim)[2];
        }

        public String getHeadIndexFromToken(String token) {
            return token.substring(token.lastIndexOf(splitTokenDelim) +1);
        }


        // Fetch root token from token list (the one with 'head-index' == 0).
        public String getRootToken(String[] tokens) {
            for (String t: tokens) {
                if (getHeadIndexFromToken(t).equals("0")) {
                    return t;
                }
            }

            // If we reached here it means no token with 'head-index' == 0 was found.
            // This means this biarc isn't valid.
            logger.severe("No head token found in biarc.");
            return null;
        }


        // Remove plural form of noun pos-label i.e. "NNS" --> "NN"
        public String getLemmatizedNounPosLabel(String posLabel) {
            // Check if last char is plural postfix i.e. "NN[S]" and remove it if so.
            if (posLabel.substring(posLabel.length() -1).equals(posLabelPlural)) {
                return posLabel.substring(0, posLabel.length() -1);
            } else {
                // Else return original pos-label.
                return posLabel;
            }
        }


        // Create token tree (graph).
        // Note the tree is actually a simple directed graph.
        // This is because we will search shortest paths between leaves.
        public SimpleDirectedGraph<String, DefaultEdge> createTree(String[] tokens) {
            // Init empty graph.
            SimpleDirectedGraph<String, DefaultEdge> g = new SimpleDirectedGraph<String, DefaultEdge>(DefaultEdge.class);

            // Init token list.
            List<String> l = new ArrayList<String>();

            // Add token vertices to tree and list.
            // The list will be used to construct edges.
            for (String t : tokens) {
                g.addVertex(t);
                l.add(t);
            }

            // Add edges between vertices and their heads.
            int i;
            for (String t : l) {
                // Fetch token head-index.
                // NOTE we substract -1 since indexes start at 1 instead at 0.
                i = Integer.parseInt(getHeadIndexFromToken(t)) -1;

                // Add edge if this vertex isn't the root vertex.
                if (i >= 0) {
                    g.addEdge(tokens[i], t);
                    g.addEdge(t, tokens[i]);  // We need the opposite edge to find shortest paths between leaves.
                }
            }

            return g;
        }


        // Get noun tokens.
        public List<String> getNounTokens(String[] tokens) {
            List<String> l = new ArrayList<String>();

            String p;
            for (String t : tokens) {
                // Return token if pos-label begins with "NN", which means it's a noun.
                p = getPosLabelFromToken(t);
                if (p.length() >= 2 && p.substring(0, 2).equals(posLabelNounPrefix)) {
                    l.add(t);
                }
            }

            return l;
        }


        // Return true if pos-label is a verb.
        public boolean isPosLabelVerb(String posLabel) {
            if (posLabel.length() >= 2 && posLabel.substring(0, 2).equals(posLabelVerbPrefix)) {
                return posLabel.substring(0, 2).equals(posLabelVerbPrefix);
            } else {
                return false;
            }
        }


        // Create dep-tree from shortest path i.e. 'danny,eating,banana' --> 'NN:subj,vv:V<eat>V,obj:NN'
        public String createDepTree(SimpleDirectedGraph<String, DefaultEdge> g, List<String> path, boolean inverse) {
            // Create a copy so we won't mutate original list.
            path = new ArrayList<String>(path);

            // Init empty string.
            StringBuilder depTree = new StringBuilder();

            // Fetch start, end vertices.
            String start = path.remove(0),
                   end = path.remove(path.size() -1),
                   posLabel, depLabel, word;

            // Build first dep-tree token i.e. 'NN:obj,'
            posLabel = getPosLabelFromToken(start);
            depTree.append(getLemmatizedNounPosLabel(posLabel)).append(depTreeDelim)
                .append(getDepLabelFromToken(start)).append(depTreeTokenDelim);

            // Add dep-tree middle tokens for middle vertices in path.
            for (String v : path) {
                posLabel = getPosLabelFromToken(v);
                depLabel = getDepLabelFromToken(v);
                word = getWordFromToken(v);

                // Append a verb middle-token. Verbs have a unique dep-tree token.
                if (isPosLabelVerb(posLabel)) {
                    depTree.append(depLabel)
                        .append(depTreeDelim)
                        .append(posLabel)
                        .append(wordDelimStart)
                        .append(inverse ? inversePrefix : "")  // Append inverse '@' prefix to word if necessary.
                        .append(stemWord(word))
                        .append(wordDelimEnd)
                        .append(posLabel)
                        .append(depTreeTokenDelim);
                } else {
                    // Append a normal middle-token if this token isn't a verb.
                    depTree.append(depLabel).append(depTreeDelim)
                        .append(posLabel).append(depTreeDelim)
                        .append(stemWord(word)).append(depTreeDelim)
                        .append(posLabel).append(depTreeTokenDelim);
                }
            }

            // Build last dep-tree token i.e. 'subj:NN'
            posLabel = getPosLabelFromToken(end);
            depTree.append(getDepLabelFromToken(end)).append(depTreeDelim)
                .append(getLemmatizedNounPosLabel(posLabel));

            return depTree.toString();
        }


        // Init 'inverse' option.
        @Override
        public void setup(Context context) {
            String r = context.getConfiguration().get(Utils.inverse, "error");
            if (r.equals("error")) {
                logger.severe("Error fetching 'inverse' argument.");
                return;
            }

            inverse = r.toLowerCase().equals(inverseArg) ? true : false ;
        }


        // TODO need to add i1, i2.
        @Override
        public void map(LongWritable key, Text value, Context context)
            throws IOException, InterruptedException {

            // Split biarcs.
            String[] ngram = value.toString().split(Utils.biarcDelim),
                tokens;

            // Fetch syntactic ngram.
            String syntacticNgram = ngram[1];

            // Split syntactic ngram to tokens.
            tokens = syntacticNgram.split(tokenDelim);

            // Fetch root token.
            String rootToken = getRootToken(tokens);

            // Create token dependency tree.
            SimpleDirectedGraph<String, DefaultEdge> tree = createTree(tokens);

            // List all noun tokens.
            List<String> nounTokens = getNounTokens(tokens);

            // Create dep-tree and emit.
            List<String> path;
            String depTree;
            // Only if there are at least two nouns to find paths between.
            if (nounTokens.size() >= 2) {
                for (String n1 : nounTokens) {
                    for (String n2 : nounTokens) {
                        // Skip paths between a node and itself.
                        if ( ! n1.equals(n2)) {
                            // Find shortest path between n1, n2.
                            path = Graphs.getPathVertexList(
                                    new DijkstraShortestPath<String, DefaultEdge> (tree, n1, n2).getPath());

                            // Only if path length <= 4
                            // (and of course path is at least of size 2, otherwise this isn't really a path).
                            if (path != null && path.size() >= 2 && path.size() <= maximumPathLength) {
                                // Set key := n1, n2
                                depTree = createDepTree(tree, path, false);

                                // TODO need to add i1, i2.
                                newKey.set(n1 + Utils.delim + n2);

                                // Set value := dep-tree, dep-tree corpus occurences (hits).
                                newValue.set(depTree + Utils.biarcDelim + ngram[2]);

                                context.write(newKey, newValue);

                                // Additionaly, create inverse dep-tree and emit,
                                // but only if root token is of 'verb' type and 'inverse' was given as argument.
                                if (inverse && isPosLabelVerb(getPosLabelFromToken(rootToken))) {
                                    // Set key := shortest path dep-tree.
                                    depTree = createDepTree(tree, Lists.reverse(path), inverse);

                                    // Set key := n2, n1  (NOTE we switch the order since this is the inverse).
                                    newKey.set(n2 + Utils.delim + n1);

                                    // Set value := inversed dep-tree, ORIGINAL dep-tree corpus occurences (hits).
                                    newValue.set(depTree + Utils.biarcDelim + ngram[2]);

                                    context.write(newKey, newValue);
                                }
                            }
                        }
                    }
                }
            }
        }
    }


    // Partition by key hash code.
    public static class PartitionerClass extends Partitioner<Text, Text> {
        @Override
        public int getPartition(Text key, Text value, int numPartitions) {
            return (key.hashCode() & Integer.MAX_VALUE) % numPartitions;
        }

    }


    // Reducer just passes on data.
    public static class ReduceClass extends Reducer<Text, Text, Text, Text> {

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
            throws IOException, InterruptedException {

            // Each (N1, N2) can have multiple dep-trees.
            for (Text value : values) {
                context.write(key, value);
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();

        conf.set(Utils.inverse, args[Utils.argInIndex]);

        Job job = new Job(conf, "Biarcs");

        job.setInputFormatClass(MySequenceFileInputFormat.class);

        job.setJarByClass(Biarcs.class);
        job.setMapperClass(MapClass.class);
        job.setPartitionerClass(PartitionerClass.class);
        job.setReducerClass(ReduceClass.class);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        // Add all but last argument as input path,
        // and append biarcs file postfix.
        String biarc;
        for (int i=0; i < 1; i++) {
            biarc = i <= 9 ? "0" + i : String.valueOf(i);
            FileInputFormat.addInputPath(job, new Path(args[Utils.argInIndex +1] + biarc + "-of-99"));
        }

        FileOutputFormat.setOutputPath(job, new Path(args[Utils.argInIndex + 2]));

        boolean result = job.waitForCompletion(true);

        // Write totalRecord and totalBytes from task counter to file.
        if (result) {
            Counters counters = job.getCounters();

            long totalRecords = counters.findCounter("org.apache.hadoop.mapred.Task$Counter", "MAP_OUTPUT_RECORDS").getValue();
            long totalBytes = counters.findCounter("org.apache.hadoop.mapred.Task$Counter", "MAP_OUTPUT_BYTES").getValue();

            Utils.uploadCountersToS3(totalRecords, totalBytes, "Biarcs");
        }

        System.exit(result ? 0 : 1);
    }
}
