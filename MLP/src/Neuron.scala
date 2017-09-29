import scala.math._

object Neuron{
  var alpha = 0D
}

class Neuron (var dimension: Int, isOutputNeuron: Boolean){
  dimension = dimension + 1 //cater for bias
//  var seed = 2//System.currentTimeMillis()
  var rand: java.util.Random =  new java.util.Random(MLP.seed)
  var inputs : Array [Double] = new Array[Double](dimension)
  var weights = new Array[Double](dimension)
  var localGradient : Double = 0
  var output : Double = 0
  var previousWeightUpdate = 0D

  def init(): Unit ={

    for(i <- 0 until dimension){
      weights(i) = -1 + (1 - (-1)) * rand.nextDouble
      weights(i) = Math.round(weights(i) * 100.0) / 100.0
    }
  }

  def setWeights(weights: Array[Double]): Unit ={
    this.weights = weights
  }

  def localField(inputs: Array[Double]): Double ={
    this.inputs = inputs
    if(isOutputNeuron){
      this.inputs = Array[Double](inputs(0), inputs(1), inputs(2), inputs(3), 1 )
    }
    return this.inputs.zip(weights).map({case (input ,weight) => input * weight}).sum// + bias
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
  def differentiatedErrorFunctionSimplified(y: Double): Double ={
    return  y * (1 - y)
  }
  def output(input: Array[Double]): Double ={
    val v = localField(input)
    output = activationFunction(v)
    return output
  }

  def errorSignal(expectedOutput: Double): Double ={
    return expectedOutput - output
  }

  def weightCorrection(learningRate: Double, inputSignal: Double, desiredOutput: Double): Double ={
    localGradient = differentiatedErrorFunctionSimplified(output) * errorSignal(desiredOutput)

    val delta = learningRate * localGradient * inputSignal
    return delta
  }

  def weightCorrection(learningRate: Double,
                       inputSignal: Double,
                       backpropagatedGradientWeightTuples: Array[(Double, Double)]): Double ={

    val weightedSumOfGradients = backpropagatedGradientWeightTuples.map({case (gradient ,weight) => gradient * weight}).sum
    localGradient = differentiatedErrorFunctionSimplified(output) * weightedSumOfGradients

    val delta = learningRate * localGradient * inputSignal
    return delta
  }

  def updateWeights(learningRate: Double, desiredOutput: Double): Unit ={
    for(i <- 0 until weights.length){
      val currentWeightUpdate = weightCorrection(learningRate, inputs(i), desiredOutput)
      weights(i) = (Neuron.alpha * previousWeightUpdate) + weights(i) + currentWeightUpdate
      previousWeightUpdate = currentWeightUpdate
    }

  }
  def updateWeights(learningRate: Double,
                    backpropagatedGradientWeightTuples: Array[(Double, Double)]): Unit ={

    for(i <- 0 until weights.length){
      val currentWeightUpdate = weightCorrection(learningRate, inputs(i), backpropagatedGradientWeightTuples)
      weights(i) = (Neuron.alpha * previousWeightUpdate) + weights(i) +  currentWeightUpdate
      previousWeightUpdate = currentWeightUpdate
    }

  }

  def getGradientWeightTuple(inputNeuronIndex: Int): (Double, Double)={
    return (localGradient, weights(inputNeuronIndex))
  }

}
