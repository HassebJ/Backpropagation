object MLP {

  val inputs = new Array[Array[Double]](16)
  var hiddenLayer = new Array[Neuron](4)
  for(i <- 0 until hiddenLayer.length){
    hiddenLayer(i) = new Neuron(4, false)
  }
  val outputLayer = Array[Neuron](new Neuron(4, true))
  val desiredOutput = new Array[Double](16)
  val absoluteErrors = new Array[Double](16)

  def init(): Unit = {
    hiddenLayer.foreach(_.init())
    outputLayer.foreach(_.init())
    inputs(0) = Array(0,0,0,0)
    inputs(1) = Array(0,0,0,1)
    inputs(2) = Array(0,0,1,0)
    inputs(3) = Array(0,0,1,1)
    inputs(4) = Array(0,1,0,0)
    inputs(5) = Array(0,1,0,1)
    inputs(6) = Array(0,1,1,0)
    inputs(7) = Array(0,1,1,1)
    inputs(8) = Array(1,0,0,0)
    inputs(9) = Array(1,0,0,1)
    inputs(10) = Array(1,0,1,0)
    inputs(11) = Array(1,0,1,1)
    inputs(12) = Array(1,1,0,0)
    inputs(13) = Array(1,1,0,1)
    inputs(14) = Array(1,1,1,0)
    inputs(15) = Array(1,1,1,1)

    for(i <- 0 until 16){
      if(inputs(i).sum % 2 == 0){
        desiredOutput(i) = 0
      }else{
        desiredOutput(i) = 1
      }
    }

//    inputs.zip(desiredOutput).foreach({case (input, output) => input.foreach(print)
//      println(s": ${output}")})
  }

//  def targetAccuracyAchieved(): Boolean ={
//    for(i<-0 until absoluteErrors.length){
//
//    }
//    val temp =
//  }
//

  def forwardPass(): Unit ={

  }

  def main(args: Array[String]): Unit = {
    init()
    val learningRate = 0.5
    var epoch = 0;
    do {

      println(s"###############    EPOCH: ${epoch}")
      epoch+=1
      inputs.zipWithIndex.foreach({ case (input, i) => {
        input.foreach(bit => print(s"${bit.toInt}, "))
        print(" : ")
        val hiddenLayerOutputs = new Array[Double](hiddenLayer.length)
        hiddenLayer.zipWithIndex.foreach({
          case (hiddenNeuron, j) => hiddenLayerOutputs(j) = hiddenNeuron.output(input)
        })


        val netOutput = outputLayer.map(outputNeuron => outputNeuron.output(input)).sum
        absoluteErrors(i) = desiredOutput(i) - netOutput
        println(s"${desiredOutput(i)} : ${netOutput} : ${absoluteErrors(i)}")


        outputLayer.foreach(_.updateWeights(learningRate, desiredOutput(i)))
        hiddenLayer.zipWithIndex.foreach({ case (hiddenNeuron, k) => {
          val gradientWeightTuple = new Array[(Double, Double)](outputLayer.length)
          outputLayer.zipWithIndex.foreach({ case (outputNeuron, l) =>
            gradientWeightTuple(l) = outputNeuron.getGradientWeightTuple(k)
          })
          hiddenNeuron.updateWeights(learningRate, gradientWeightTuple)
        }
        })


      }
      })
    }while(!absoluteErrors.map(_ match {
      case x if x >= 0.05 =>
        true
      case _ =>
        false
    }).forall(identity))
  }



}
