
import java.util.Random;

public class XOR_Example
{
    private static final int NUM_INPUTS = 5;      // Input nodes, plus the bias input.
    private static final int NUM_PATTERNS = 16;    // Input patterns for XOR experiment.

    private static final int NUM_HIDDEN = 4;
    private static final int NUM_EPOCHS = 200;
    private static final double LR_IH = 0.05;      // Learning rate, input to hidden weights.
    private static final double LR_HO = LR_IH;     // Learning rate, hidden to output weights.

    private static double hiddenVal[] = new double[NUM_HIDDEN];    // Hidden node outputs.

    private static double weightsIH[][] =  new double[NUM_INPUTS][NUM_HIDDEN]; // Input to Hidden weights.
    private static double weightsHO[] = new double[NUM_HIDDEN];    // Hidden to Output weights.

    private static int trainInputs[][] = new int[NUM_PATTERNS][NUM_INPUTS];
    private static int trainOutput[] = new int[NUM_PATTERNS];      // "Actual" output values.
    private static double netOutput[] = new double[NUM_PATTERNS];
    private static double errorOutput[] = new double[NUM_PATTERNS];

    private static double errThisPat = 0.0;
    private static double outPred = 0.0;     // "Expected" output values.
    private static double RMSerror = 0.0;    // Root Mean Squared error.

    private static void algorithm()
    {
        int patNum = 0;

        initWeights();
        initData();

        // Train the network.
        int j = 0;

        do{
            for(int i = 0; i < NUM_PATTERNS; i++)
            {
                // Select a pattern at random.
                patNum = i;//new Random().nextInt(16);

                // Calculate the output and error for this pattern.
                calcNet(patNum);

                // Adjust network weights.
                WeightChangesHO();
                WeightChangesIH(patNum);

            } // i
            RMSerror = calcOverallError();

            // Display the overall network error after each epoch
            System.out.println("epoch = " + j + " RMS Error = " + RMSerror);

            j++;
        }while(requiredAccuracyAchieved() == false); // j

        System.out.println("NUMBER OF EPOCHS REQUIRED: " + j);

        displayResults();

        return;
    }

    static boolean requiredAccuracyAchieved(){
//        int count = 0;
        for (int i = 0; i < errorOutput.length; i++){
            if(Math.abs(errorOutput[i]) > 0.05){
                return false;
            }
        }
        return true;
    }

    private static void initWeights()
    {
        //  Initialize weights to random values.
        for(int j = 0; j < NUM_HIDDEN; j++)
        {
            weightsHO[j] = -1 + (1 - (-1)) * new Random().nextDouble();
            for(int i = 0; i < NUM_INPUTS; i++)
            {
                weightsIH[i][j] = -1 + (1 - (-1)) * new Random().nextDouble();
                System.out.println("Weight = " + weightsIH[i][j]);
            } // i
        } // j

        return;
    }

    private static void initData()
    {
        // The data here is the XOR data which has been rescaled to the range -1 to 1.
        // An extra input value of 1 is also added to act as the bias.
        // The output must lie in the range -1 to 1.

        trainInputs[0][0] = -1;
        trainInputs[0][1] = -1;
        trainInputs[0][2] = -1;
        trainInputs[0][3] = -1;

        trainInputs[0][4] = 1;    // Bias
        trainOutput[0] = 0;

        trainInputs[1][0] = -1;
        trainInputs[1][1] = -1;
        trainInputs[1][2] = -1;
        trainInputs[1][3] = 1;

        trainInputs[1][4] = 1;    // Bias
        trainOutput[1] = 1;

        trainInputs[2][0] = -1;
        trainInputs[2][1] = -1;
        trainInputs[2][2] = 1;
        trainInputs[2][3] = -1;

        trainInputs[2][4] = 1;    // Bias
        trainOutput[2] = 1;

        trainInputs[3][0] = -1;
        trainInputs[3][1] = -1;
        trainInputs[3][2] = 1;
        trainInputs[3][3] = 1;

        trainInputs[3][4] = 1;    // Bias
        trainOutput[3] = 0;

        trainInputs[4][0] = -1;
        trainInputs[4][1] = 1;
        trainInputs[4][2] = -1;
        trainInputs[4][3] = -1;

        trainInputs[4][4] = 1;    // Bias
        trainOutput[4] = 1;

        trainInputs[5][0] = -1;
        trainInputs[5][1] = 1;
        trainInputs[5][2] = -1;
        trainInputs[5][3] = 1;

        trainInputs[5][4] = 1;    // Bias
        trainOutput[5] = 0;

        trainInputs[6][0] = -1;
        trainInputs[6][1] = 1;
        trainInputs[6][2] = 1;
        trainInputs[6][3] = -1;

        trainInputs[6][4] = 1;    // Bias
        trainOutput[6] = 0;

        trainInputs[7][0] = -1;
        trainInputs[7][1] = 1;
        trainInputs[7][2] = 1;
        trainInputs[7][3] = 1;

        trainInputs[7][4] = 1;    // Bias
        trainOutput[7] = 1;







        trainInputs[8][0] = 1;
        trainInputs[8][1] = -1;
        trainInputs[8][2] = -1;
        trainInputs[8][3] = -1;

        trainInputs[8][4] = 1;    // Bias
        trainOutput[8] = 1;

        trainInputs[9][0] = 1;
        trainInputs[9][1] = -1;
        trainInputs[9][2] = -1;
        trainInputs[9][3] = 1;

        trainInputs[9][4] = 1;    // Bias
        trainOutput[9] = 0;

        trainInputs[10][0] = 1;
        trainInputs[10][1] = -1;
        trainInputs[10][2] = 1;
        trainInputs[10][3] = -1;

        trainInputs[10][4] = 1;    // Bias
        trainOutput[10] = 0;

        trainInputs[11][0] = 1;
        trainInputs[11][1] = -1;
        trainInputs[11][2] = 1;
        trainInputs[11][3] = 1;

        trainInputs[11][4] = 1;    // Bias
        trainOutput[11] = 1;

        trainInputs[12][0] = 1;
        trainInputs[12][1] = 1;
        trainInputs[12][2] = -1;
        trainInputs[12][3] = -1;

        trainInputs[12][4] = 1;    // Bias
        trainOutput[12] = 0;

        trainInputs[13][0] = 1;
        trainInputs[13][1] = 1;
        trainInputs[13][2] = -1;
        trainInputs[13][3] = 1;

        trainInputs[13][4] = 1;    // Bias
        trainOutput[13] = 1;

        trainInputs[14][0] = 1;
        trainInputs[14][1] = 1;
        trainInputs[14][2] = 1;
        trainInputs[14][3] = 0;

        trainInputs[14][4] = 1;    // Bias
        trainOutput[14] = 1;

        trainInputs[15][0] = 1;
        trainInputs[15][1] = 1;
        trainInputs[15][2] = 1;
        trainInputs[15][3] = 1;

        trainInputs[15][4] = 1;    // Bias
        trainOutput[15] = 0;

        return;
    }

    private static void calcNet(final int patNum)
    {
        // Calculates values for Hidden and Output nodes.
        for(int i = 0; i < NUM_HIDDEN; i++)
        {
            hiddenVal[i] = 0.0;
            for(int j = 0; j < NUM_INPUTS; j++)
            {
                hiddenVal[i] += (trainInputs[patNum][j] * weightsIH[j][i]);
            } // j
//            hiddenVal[i] = Math.tanh(hiddenVal[i]);
            hiddenVal[i] = 1/(1 + Math.exp(-hiddenVal[i]));
        } // i

        outPred = 0.0;

        for(int i = 0; i < NUM_HIDDEN; i++)
        {
            outPred += hiddenVal[i] * weightsHO[i];

        }
        netOutput[patNum] = outPred;

        errThisPat = outPred - trainOutput[patNum]; // Error = "Expected" - "Actual"
        errorOutput[patNum] = errThisPat;
         return;
    }

    private static void WeightChangesHO()
    {
        // Adjust the Hidden to Output weights.
        for(int k = 0; k < NUM_HIDDEN; k++)
        {
            double weightChange = LR_HO * errThisPat * hiddenVal[k];
            weightsHO[k] -= weightChange;

            // Regularization of the output weights.
            if(weightsHO[k] < -5.0){
                weightsHO[k] = -5.0;
            }else if(weightsHO[k] > 5.0){
                weightsHO[k] = 5.0;
            }
        }
        return;
    }

    private static void WeightChangesIH(final int patNum)
    {
        // Adjust the Input to Hidden weights.
        for(int i = 0; i < NUM_HIDDEN; i++)
        {
            for(int k = 0; k < NUM_INPUTS; k++)
            {
//                double x = 1 - Math.pow(hiddenVal[i], 2);
                double x = hiddenVal[i] * (1 - hiddenVal[i]);
                x = x * weightsHO[i] * errThisPat * LR_IH;
                x = x * trainInputs[patNum][k];

                double weightChange = x;
                weightsIH[k][i] -= weightChange;
            } // k
        } // i
        return;
    }

    private static double calcOverallError()
    {
        double errorValue = 0.0;

        for(int i = 0; i < NUM_PATTERNS; i++)
        {
            calcNet(i);
            errorValue += Math.pow(errThisPat, 2);
        }

        errorValue /= NUM_PATTERNS;

        return Math.sqrt(errorValue);
    }

    private static void displayResults()
    {
        for(int i = 0; i < NUM_PATTERNS; i ++)
        {
            calcNet(i);
            System.out.println("pat = " + (i + 1) + " actual = " + trainOutput[i] + " neural model = " + outPred);
        }
        return;
    }

    public static void main(String[] args)
    {
        algorithm();
        return;
    }

}