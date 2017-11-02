import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
//import libsvm.

public  class SVM {

    static List<DataPoint> trainingData = new ArrayList<>();
    static List<DataPoint> testData = new ArrayList<>();
    static svm_train lib_svm = new svm_train();


    public static void main(String [] arg){
        SVM svm = new SVM();
        svm.loadData();
        svm.readProblemFromFile("SVM/src/data/ncrna_s.train");



        for(DataPoint p: testData){
            System.out.println(p);
        }
    }

    public void readProblemFromFile(String filename){
        lib_svm.


    }

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
}


