

import libsvm.svm_problem;

import javax.swing.plaf.synth.SynthDesktopIconUI;
import java.io.*;
import java.util.*;
import java.util.function.Function;


public  class SVM {

    static List<DataPoint> trainingData = new ArrayList<>();
    static List<DataPoint> testData = new ArrayList<>();
    static svm_train lib_svm = new svm_train();
    static Random rand = new Random(7);


    public static void main(String [] arg) throws IOException{

        testWithLinearKernel();

        crossValidateAndTrainWithBestParamsWithRBF();

    }

    public static void testWithLinearKernel(){
        StringBuilder builder = new StringBuilder();
        builder.append("c , accuracy \n");
        for(int c = -4; c <= 8; c++) {
            double constraint = Math.pow(2, c);
            System.out.println("############ Running with C: 2^"+c+" = " + constraint + "#############");
            double accuracy = trainAndTestWithC(constraint);
            System.out.println("Accuracy: " + accuracy);
            builder.append(c+", "+ accuracy+ "\n");
        }
        System.out.println(builder.toString());
    }

    public static void crossValidateAndTrainWithBestParamsWithRBF() throws IOException{

        StringBuilder builder = new StringBuilder();

        double [] maxAccuracyTriple = new double [3];
        maxAccuracyTriple[0] = Double.MIN_VALUE;

        for(int alpha = -4; alpha <= 8; alpha++) {
            for(int c = -4; c <= 8; c++) {
                double constraint = Math.pow(2, c);
                double floatingAlpha = Math.pow(2, alpha);

                System.out.println("############ Cross Validating with C: 2^"+c+" = "
                        + constraint
                        + " & alpha: 2^"+ alpha +" = "
                        + floatingAlpha
                        +"#############");

                double cvAccuracy = crossValidate(5,floatingAlpha, constraint);
                System.out.println("Average Cross Validation Accuracy: " + cvAccuracy);

                if(cvAccuracy > maxAccuracyTriple[0] ) {
                    maxAccuracyTriple[0] = cvAccuracy;
                    maxAccuracyTriple[1] = c;
                    maxAccuracyTriple[2] = alpha;
                }


                String printString = String.valueOf(cvAccuracy);
                if(c < 8){
                    printString = printString + ", ";
                    builder.append(printString);
//                    System.out.print(printString);
                }else {
                    builder.append(printString);
                    builder.append("\n");
//                    System.out.println(printString);
                }
            }
        }
        System.out.println("\n********************  cvAccuracyMatrix **************");
        System.out.println(builder.toString());
        System.out.println("********************   **************");

        System.out.println("Max CV Accuracy: " + maxAccuracyTriple[0]
                + "with C : 2^" + maxAccuracyTriple[1]
                + " and Alpha: 2^" + maxAccuracyTriple[2]);

        double constraint = Math.pow(2, maxAccuracyTriple[1]);
        double floatingAlpha = Math.pow(2, maxAccuracyTriple[2]);
        String args = new String("-s 0 -t 2 -c "+ constraint
                +" -g " + floatingAlpha
                + " SVM/src/data/ncrna_s.train");

        executeMethodAndGetOutput(SVM::trainWithArgs, args.split(" "));
        double bestGloabalAccuracy = predictAndGetAccuracy(null, null);
        System.out.println("bestGloabalAccuracy: " + bestGloabalAccuracy);
    }
    public static double crossValidate(int foldValue, double alpha, double c) throws  IOException {
        String [] trainingDataSet = getTrainingData();
        int dataSetSize = trainingDataSet.length;
        int cvStartIndex = rand.nextInt(dataSetSize/2);
        int cvEndIndex = cvStartIndex + dataSetSize/2;

        String [] crossValidationSet = Arrays.copyOfRange(trainingDataSet, cvStartIndex, cvEndIndex);

        int subSetSize = crossValidationSet.length/foldValue;

        String [] subSetFilenames = new String[foldValue];
        for(int i = 0; i < foldValue; i++){
            subSetFilenames[i] = "cv_subset" + i +".train";
            writeSubProblemToFile(crossValidationSet,
                    i * subSetSize ,
                    i * subSetSize + subSetSize,
                    subSetFilenames[i]);
        }

        double aggreagtedAccuracy = 0.0;
        for (int i = 0; i < foldValue; i++){
            String currentTrainingDataSet = concatenateTrainigFilesExcluding(i, subSetFilenames);
            String args = new String("-s 0 -t 2 -c "+ c +" -g " + alpha+ " " + currentTrainingDataSet);

            executeMethodAndGetOutput(SVM::trainWithArgs, args.split(" "));
//            trainWithArgs(args.split(" "));

            double accuracy = predictAndGetAccuracy(subSetFilenames[i],currentTrainingDataSet+".model");
//            System.out.println("Trained With: " + currentTrainingDataSet
//                    + " Predicting With: " + subSetFilenames[i]
//                    + " Accuracy: " + accuracy);
            aggreagtedAccuracy+=accuracy;
        }
        double crossValidationAccuracy = aggreagtedAccuracy / foldValue;
        return crossValidationAccuracy;
    }

    static String concatenateTrainigFilesExcluding(int subSetTestIndex, String [] subSetFilenames) throws IOException{
        String concatedFilename = "cv_subset_wo" + subSetTestIndex +".train";
        OutputStream out = new FileOutputStream(concatedFilename);
//        System.out.print("Concating files: ");
        byte[] buf = new byte[1024];
        for (int i = 0; i < subSetFilenames.length; i++) {
            if(i == subSetTestIndex){
                continue;
            }
            String file = subSetFilenames[i];
//            System.out.println(file);
            InputStream in = new FileInputStream(file);
            int b = 0;
            while ( (b = in.read(buf)) >= 0) {
                out.write(buf, 0, b);
                out.flush();
            }
        }
        out.close();
        return concatedFilename;
    }

    static String executeMethodAndGetOutput(Function<String [], Void> func, String [] args){
        // Create a stream to hold the output
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        // IMPORTANT: Save the old System.out!
        PrintStream old = System.out;
        // Tell Java to use your special stream
        System.setOut(ps);
        // Print some output: goes to your special stream

        func.apply(args);

        System.out.flush();
        System.setOut(old);
        // Show what happened
//        System.out.println("Here: " + baos.toString());
        String output = baos.toString();
        return output;
    }

    private static double predictAndGetAccuracy(String testFile, String modelFile){
//        // Create a stream to hold the output
//        ByteArrayOutputStream baos = new ByteArrayOutputStream();
//        PrintStream ps = new PrintStream(baos);
//        // IMPORTANT: Save the old System.out!
//        PrintStream old = System.out;
//        // Tell Java to use your special stream
//        System.setOut(ps);
//        // Print some output: goes to your special stream
        String [] args = {testFile, modelFile};
        String output = executeMethodAndGetOutput(SVM::predictWith, args);
//        // Put things back
//        System.out.flush();
//        System.setOut(old);
//        // Show what happened
////        System.out.println("Here: " + baos.toString());
//        String output = baos.toString();
        return Double.parseDouble(output.substring(output.indexOf("=") + 1, output.indexOf("%")));
    }

    private static svm_problem getSubProblem(svm_problem dataSet, int start, int end){
        svm_problem subSet = new svm_problem();
        subSet.y = Arrays.copyOfRange(dataSet.y, start, end);
        subSet.x = Arrays.copyOfRange(dataSet.x, start, end);
        subSet.l = subSet.y.length;
        return subSet;
    }

    private static String[] writeSubProblemToFile(String [] dataSet, int start, int end, String filename) throws IOException{
        String [] subSet = Arrays.copyOfRange(dataSet, start, end);
        File subSetFile = new File(filename);
        PrintWriter writer = new PrintWriter(subSetFile);

        for(String line: subSet){
            writer.println(line);
        }
        writer.close();
        return subSet;
    }

    public static double trainAndTestWithC(Double c){
        String[] arguments = new String[] {"-s", "0", "-t", "0", "-c", c.toString(), "SVM/src/data/ncrna_s.train"};
            executeMethodAndGetOutput(SVM::trainWithArgs, arguments);
            return predictAndGetAccuracy(null,null);

    }

    private static Void trainWithArgs(String [] arguments){
        try {
            lib_svm.main(arguments);
        }catch (IOException e){
            e.printStackTrace();
        }
        return null;
    }

    public static Void predictWith(String [] args){
        String testFile = args[0];
        if(testFile == null){
            testFile = "SVM/src/data/ncrna_s.test";
        }
        String modelFile = args[1];
        if(modelFile == null){
            modelFile = "ncrna_s.train.model";
        }
        try {
            String [] arguments = new String[] {testFile, modelFile, "ncrna_s.out"};
            svm_predict.main(arguments);
        }catch (IOException e){
            e.printStackTrace();
        }
        return null;
    }

    public static String [] getTrainingData(){
        List<String> tempList = new ArrayList<>();
        File trainingFile = new File("SVM/src/data/ncrna_s.train");

        try {
            Scanner fileReader = new Scanner(trainingFile);
            while (fileReader.hasNext()) {
                tempList.add(fileReader.nextLine());
//                trainingData.add(getDataPointFromString(fileReader.nextLine()));
            }
        }catch (Exception e){
            e.printStackTrace();
        }

        return tempList.toArray(new String[0]);

    }

    //    public static void crossValidate(int foldValue, double alpha, double c) throws  IOException{
//        svm_problem trainingDataSet = lib_svm.read_problem_with_file("SVM/src/data/ncrna_s.train", alpha);
//        int dataSetSize = trainingDataSet.y.length;
//        Random rand = new Random();
//        int cvStartIndex = rand.nextInt(dataSetSize/2);
//        int cvEndIndex = cvStartIndex + dataSetSize/2;
//
//        svm_problem crossValidationSet = getSubProblem(trainingDataSet, cvStartIndex, cvEndIndex);
//
//        int subSetSize = crossValidationSet.l / foldValue;
//
//        svm_problem [] subSets = new svm_problem[foldValue];
//        for(int i = 0; i < foldValue; i++){
//            subSets[i] = getSubProblem(crossValidationSet, i * subSetSize ,i * subSetSize + subSetSize);
//            String args = new String("-s 0 -t 2 -c "+ c +" -g " + alpha+ " SVM/src/data/ncrna_s.train");
//            trainWithArgs(args.split(" "));
//            System.out.println(predictAndGetConsoleOutput())
//        }
//
//    }

    public static void loadData(){
        File trainingFile = new File("SVM/src/data/ncrna_s.train");
        File testFile = new File("SVM/src/data/ncrna_s.test");

        try {
            Scanner fileReader = new Scanner(trainingFile);
            while(fileReader.hasNext()){
                trainingData.add(getDataPointFromString(fileReader.nextLine()));
            }
            fileReader.close();

            fileReader = new Scanner(testFile);
            while(fileReader.hasNext()){
                testData.add(getDataPointFromString(fileReader.nextLine()));
            }
            fileReader.close();
        }catch(FileNotFoundException e){
            System.out.println("Probably file not found. Exiting...");
            System.exit(-1);
        }
    }

    private static DataPoint getDataPointFromString(String line) {
        String[] features = line.split(" ");
        DataPoint point = new DataPoint();
        point.desired = Double.parseDouble(features[0]);

        int featureCounter = 1;
        for (int i = featureCounter; i <= 8; i++) {
            if (i >= features.length) {
                break;
            }
            String[] currentFeature = features[i].split(":");
            int currentFeatureIndex = Integer.parseInt(currentFeature[0]);
            double currentFeatureValue = Double.parseDouble(currentFeature[1]);
            switch (currentFeatureIndex) {
                case 1:
                    point.dynalign = currentFeatureValue;
                    break;
                case 2:
                    point.shortSeqLength = currentFeatureValue;
                    break;
                case 3:
                    point.aFreqOfSeqOne = currentFeatureValue;
                    break;
                case 4:
                    point.uFreqOfSeqOne = currentFeatureValue;
                    break;
                case 5:
                    point.cFreqOfSeqOne = currentFeatureValue;
                    break;
                case 6:
                    point.aFreqOfSeqTwo = currentFeatureValue;
                    break;
                case 7:
                    point.uFreqOfSeqTwo = currentFeatureValue;
                    break;
                case 8:
                    point.cFreqOfSeqTwo = currentFeatureValue;
                    break;
            }
        }

        return point;
    }

    class Foo {
        public String f() { return "f"; }
        public String g() { return "g"; }
        // ...
    }

    public List<String> collect(List<Foo> foos, Function<Foo,String> func)
    {
//        collect(foos, Foo::f);
        List<String> result = new ArrayList<String>();

        for (final Foo foo: foos) {
            result.add(func.apply(foo));
        }

        return result;
    }
}


