object MLP {

  val inputs = new Array[Array[Double]](16)
  var hiddenLayer = new Array[Neuron](3)
  for(i <- 0 until hiddenLayer.length){
    hiddenLayer(i) = new Neuron(2, false)
  }
  val outputLayer = Array[Neuron](new Neuron(3, true))
  val desiredOutput = new Array[Double](16)
  val absoluteErrors = new Array[Double](16)
  val netOutputs = new Array[Double](16)

  def init(): Unit = {
//    hiddenLayer.foreach(_.init())
//    var weights = Array[Double](0.8, 0.2)
    hiddenLayer(0).setWeights(Array[Double](0.8, 0.2))
    hiddenLayer(1).setWeights(Array[Double](0.4, 0.9))
    hiddenLayer(2).setWeights(Array[Double](0.3, 0.5))

//    outputLayer.foreach(_.init())
    outputLayer(0).setWeights(Array[Double](0.3, 0.5, 0.9))
    inputs(0) = Array(1,1)
//    inputs(0) = Array(0,0,0,0)
//    inputs(1) = Array(0,0,0,1)
//    inputs(2) = Array(0,0,1,0)
//    inputs(3) = Array(0,0,1,1)
//    inputs(4) = Array(0,1,0,0)
//    inputs(5) = Array(0,1,0,1)
//    inputs(6) = Array(0,1,1,0)
//    inputs(7) = Array(0,1,1,1)
//    inputs(8) = Array(1,0,0,0)
//    inputs(9) = Array(1,0,0,1)
//    inputs(10) = Array(1,0,1,0)
//    inputs(11) = Array(1,0,1,1)
//    inputs(12) = Array(1,1,0,0)
//    inputs(13) = Array(1,1,0,1)
//    inputs(14) = Array(1,1,1,0)
//    inputs(15) = Array(1,1,1,1)


    desiredOutput(0) = 0
//    for(i <- 0 until 16){
//      if(inputs(i).sum % 2 == 0){
//        desiredOutput(i) = 0
//      }else{
//        desiredOutput(i) = 1
//      }
//    }
  }


  def forwardPass(): Unit ={

  }

  def main(args: Array[String]): Unit = {
    init()
    val learningRate = 1.5
    var epoch = 0
    do {
      if(epoch % 1000 == 0) {
        println(s"###############    EPOCH: ${epoch}")
      }

      inputs.zipWithIndex.foreach({ case (input, i) => {

        val hiddenLayerOutputs = new Array[Double](hiddenLayer.length)
        hiddenLayer.zipWithIndex.foreach({
          case (hiddenNeuron, j) =>
            hiddenLayerOutputs(j) = hiddenNeuron.output(input)
            println(s"hidden result ${j+1} = ${hiddenLayerOutputs(j)}")
        })


        netOutputs(i) = outputLayer.map(outputNeuron => outputNeuron.output(hiddenLayerOutputs)).sum
        absoluteErrors(i) = Math.abs(desiredOutput(i) - netOutputs(i))

        println(s"output result = ${netOutputs(i)}")

        if(epoch % 1000 == 0) {

          input.foreach(bit => print(s"${bit.toInt}, "))
          print(": ")
          println(s"${desiredOutput(i).toInt} : ${netOutputs(i)} : ${absoluteErrors(i)}")
        }


        outputLayer.foreach(_.updateWeights(learningRate, desiredOutput(i)))
        hiddenLayer.zipWithIndex.foreach({ case (hiddenNeuron, k) => {
          val gradientWeightTuple = new Array[(Double, Double)](outputLayer.length)
          outputLayer.zipWithIndex.foreach({ case (outputNeuron, l) =>
            gradientWeightTuple(l) = outputNeuron.getGradientWeightTuple(k)
          })
          hiddenNeuron.updateWeights(learningRate, gradientWeightTuple)
        }
        })

        return
      }
      })

      epoch+=1
//      scala.io.StdIn.readLine()
      return
    }while(!absoluteErrors.map(_ match {
      case x if Math.abs(x) <= 0.05 =>
        true
      case _ =>
        false
    }).forall(identity))

    println(s"###############    EPOCH: ${epoch}")
    inputs.zipWithIndex.foreach({ case (input, i) => {
      input.foreach(bit => print(s"${bit.toInt}, "))
      print(" : ")
      println(s"${desiredOutput(i)} : ${netOutputs(i)} : ${absoluteErrors(i)}")
    }})


  }

}
