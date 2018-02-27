import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

object MLP{
  val seed = 0
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    var pool: ExecutorService = Executors.newFixedThreadPool(4)
//
    for(i <- 0.05 to 0.5 by 0.05){
        val mlp = new MLP(Math.round(i * 100.0) / 100.0, 0);
      mlp.algorithm(i)
    }

    for(i <- 0.05 to 0.5 by 0.05){
      val mlp = new MLP(Math.round(i * 100.0) / 100.0, 0.9);
      mlp.algorithm(i)
    }

    val end = System.currentTimeMillis()
    println(s"Time taken: ${(end-start)/1000/60} minutes")

  }
}

class MLP(learningRate: Double, alpha: Double) extends Runnable{

  val inputs = new Array[Array[Double]](16)
  var hiddenLayer = new Array[Neuron](4)
  for(i <- 0 until hiddenLayer.length){
    hiddenLayer(i) = new Neuron(4, false)
  }
  val outputLayer = Array[Neuron](new Neuron(4, true))
  val desiredOutput = new Array[Double](16)
  val absoluteErrors = new Array[Double](16)
  val netOutputs = new Array[Double](16)

  def init(): Unit = {
    outputLayer.foreach(_.init())
    hiddenLayer.foreach(_.init())

    inputs(0) = Array(0,0,0,0,1)
    inputs(1) = Array(0,0,0,1,1)
    inputs(2) = Array(0,0,1,0,1)
    inputs(3) = Array(0,0,1,1,1)
    inputs(4) = Array(0,1,0,0,1)
    inputs(5) = Array(0,1,0,1,1)
    inputs(6) = Array(0,1,1,0,1)
    inputs(7) = Array(0,1,1,1,1)
    inputs(8) = Array(1,0,0,0,1)
    inputs(9) = Array(1,0,0,1,1)
    inputs(10) = Array(1,0,1,0,1)
    inputs(11) = Array(1,0,1,1,1)
    inputs(12) = Array(1,1,0,0,1)
    inputs(13) = Array(1,1,0,1,1)
    inputs(14) = Array(1,1,1,0,1)
    inputs(15) = Array(1,1,1,1,1)

    for(i <- 0 until 16){
      if(inputs(i).sum % 2 == 0){
        desiredOutput(i) = 1
      }else{
        desiredOutput(i) = 0
      }
    }
  }

  override def run(): Unit = {
//    println(s"Starting with n: ${learningRate}, a: ${alpha}" )
    algorithm(learningRate)
  }

  def algorithm(learningRate: Double): Unit = {
    init()
    var epoch = 0;
    do {
      inputs.zipWithIndex.foreach({ case (input, i) => {

        val hiddenLayerOutputs = new Array[Double](hiddenLayer.length)
        hiddenLayer.zipWithIndex.foreach({
          case (hiddenNeuron, j) => hiddenLayerOutputs(j) = hiddenNeuron.output(input)
        })

        netOutputs(i) = outputLayer.map(outputNeuron => outputNeuron.output(hiddenLayerOutputs)).sum
        absoluteErrors(i) = desiredOutput(i) - netOutputs(i)
        outputLayer.foreach(_.updateWeights(learningRate, desiredOutput(i)))

        val gradientWeightTuples = Array.ofDim[(Double, Double)](hiddenLayer.length, outputLayer.length)
        hiddenLayer.zipWithIndex.foreach({ case (hiddenNeuron, k) => {
          val gradientWeightTuple = new Array[(Double, Double)](outputLayer.length)
          outputLayer.zipWithIndex.foreach({ case (outputNeuron, l) =>
            gradientWeightTuple(l) = outputNeuron.getGradientWeightTuple(k)
          })
          gradientWeightTuples(k) = gradientWeightTuple
        }
        })

        hiddenLayer.zip(gradientWeightTuples).foreach({ case (hiddenNeuron, gradientWeightTuple) => {
          hiddenNeuron.updateWeights(learningRate, gradientWeightTuple)
        }})
      }
      })
      epoch+=1

    }while(!absoluteErrors.map(_ match {
      case x if Math.abs(x) <= 0.05 =>
        true
      case _ =>
        false
    }).forall(identity))
    println(s"Epochs: ${epoch} -  LearningRate: ${learningRate} - MomentumConstant: ${Neuron.alpha} - Seed: ${MLP.seed}" )
  }



}
