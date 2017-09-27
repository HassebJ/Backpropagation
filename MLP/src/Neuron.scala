import scala.math._

class Neuron (dimension: Int, isOutputNeuron: Boolean){
  var inputs : Array [Double] = new Array[Double](dimension)
  var weights = new Array[Double](dimension)
  var bias : Double = 0
  var localGradient : Double = 0
  var output : Double = 0
//  var desiredOutput: Double


  def init(): Unit ={
    val rand = scala.util.Random
    rand.setSeed(50)
    for(i <- 0 until dimension){
//      inputs(i) = rand.nextDouble()
      weights(i) = -1 + (1 - (-1)) * rand.nextDouble
      bias = -1 + (1 - (-1)) * rand.nextDouble
    }
  }



  def localField(inputs: Array[Double]): Double ={
    this.inputs = inputs
    return this.inputs.zip(weights).map({case (input ,weight) => input * weight}).sum + bias
  }

  def activationFunction(v: Double): Double ={
    val a = 1
    return 1/(1 + exp(-a * v))
  }

  def differentiatedErrorFunction(v: Double): Double ={
    val a = 1
    val numerator = a * exp(-a * v)
    val denominator = Math.pow((1 + exp(-a * v)), 2)
    return  numerator/denominator

  }

  def output(input: Array[Double]): Double ={
    output = activationFunction(localField(input))
    return output
  }

  def errorSignal(expectedOutput: Double): Double ={
    return expectedOutput - output
  }

  def weightCorrection(learningRate: Double, inputSignal: Double, desiredOutput: Double): Double ={
    localGradient = differentiatedErrorFunction(output) * errorSignal(desiredOutput)
//    print(s"Local gradient: ${localGradient}")

    return learningRate * localGradient * inputSignal
  }

  def updateWeights(learningRate: Double, desiredOutput: Double): Unit ={
    //    val error = weightCorrection(learningRate, desiredOutput)
    for(i <- 0 until weights.length){
      weights(i) = weights(i) + weightCorrection(learningRate, inputs(i), desiredOutput)
    }
    bias = bias + weightCorrection(learningRate, desiredOutput, 1)

//    println(weights)
  }

  def weightCorrection(learningRate: Double,
                       inputSignal: Double,
                       backpropagatedGradientWeightTuples: Array[(Double, Double)]): Double ={

    val weightedSumOfGradients = backpropagatedGradientWeightTuples.map({case (gradient ,weight) => gradient * weight}).sum
    localGradient = differentiatedErrorFunction(output) * weightedSumOfGradients
    return learningRate * localGradient * inputSignal
  }


  def updateWeights(learningRate: Double,
                    backpropagatedGradientWeightTuples: Array[(Double, Double)]): Unit ={

    for(i <- 0 until weights.length){
      weights(i) = weights(i) + weightCorrection(learningRate, inputs(i), backpropagatedGradientWeightTuples)
    }
    bias = bias + weightCorrection(learningRate, 1, backpropagatedGradientWeightTuples)


  }

  def getGradientWeightTuple(inputNeuronIndex: Int): (Double, Double)={
    return (localGradient, weights(inputNeuronIndex))
  }

}
