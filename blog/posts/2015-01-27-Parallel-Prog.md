---
title: The problem with Parallel Programming
description: 
tags: Programming
---

Parallel programming is hard.
The main problem for me is that parallel programming mixes two different things: the logic of a program, and its run-time behavior, together in the same language.
In traditional programming, the programmer usually doesn't care about the run-time evaluation strategy of her program.
If the language is imperative, the evaluation will be a step by step evaluation and execution of the instructions of the program.
If it's a functional language, the functions will be plugged together like pipes as a first step, and then the data will flow.
But the programmer didn't chose the model: it was imposed by the language framework.

However with parallel programming everything becomes more complicated.
Take a simple problem of counting words in a text.
Here is the standard implementation with Hadoop ([source](http://wiki.apache.org/hadoop/WordCount)):

```Java
public class WordCount {
           
    public static class Map extends Mapper<LongWritable, Text, Text, IntWritable> {
       private final static IntWritable one = new IntWritable(1);
       private Text word = new Text();
           
       public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
           String line = value.toString();
           StringTokenizer tokenizer = new StringTokenizer(line);
           while (tokenizer.hasMoreTokens()) {
               word.set(tokenizer.nextToken());
               context.write(word, one);
           }
       }
    } 
           
    public static class Reduce extends Reducer<Text, IntWritable, Text, IntWritable> {
   
       public void reduce(Text key, Iterable<IntWritable> values, Context context) 
         throws IOException, InterruptedException {
           int sum = 0;
           for (IntWritable val : values) {
               sum += val.get();
           }
           context.write(key, new IntWritable(sum));
       }
    }
           
    public static void main(String[] args) throws Exception {
       Configuration conf = new Configuration();
           
           Job job = new Job(conf, "wordcount");
       
       job.setOutputKeyClass(Text.class);
       job.setOutputValueClass(IntWritable.class);
           
       job.setMapperClass(Map.class);
       job.setReducerClass(Reduce.class);
           
       job.setInputFormatClass(TextInputFormat.class);
       job.setOutputFormatClass(TextOutputFormat.class);
           
       FileInputFormat.addInputPath(job, new Path(args[0]));
       FileOutputFormat.setOutputPath(job, new Path(args[1]));
           
       job.waitForCompletion(true);
    }
}
```

The example is quite long, for just counting words!
Maybe it's a side effect of Java and Hadoop technology choices?
Here is the same example in Haskell ([source](https://github.com/Soostone/hadron/blob/master/examples/WordCount.hs)):

```Haskell
main :: IO ()
main = mapReduceMain def pSerialize mapper' reducer'

mapper':: Mapper B.ByteString CompositeKey Int
mapper' = linesConduit =$= C.concatMap f
    where f ln = map (\w -> ([w], 1 :: Int)) $ B.words ln

reducer':: Reducer CompositeKey Int B.ByteString
reducer' = do
    (w, cnt) <- C.fold (\ (_, cnt) ([k], x) -> (k, cnt + x)) ("", 0)
    yield $ B.concat [rowToStr def [w, B.pack . show $ cnt], "\n"]
```

It's a bit shorter of course.
But way longer and obfuscated than the non-parallel version:

    wordCount :: String -> [(String, Int)]
    wordCount = map (head &&& length) . group . sort . words

The real problem with parallel programming is that the program mixes up program logic and program run time behavior description.
The program logic (here, counting words) is broken up and drown into a swamp of runtime behavior code.
All parallelization frameworks that I surveyed implied to include those run-time annotations.
How to achieve the design goal of keeping the logic of a program separated from its run time behavior indications?
This is the real question that should be solved in parallel programming in my opinion.


