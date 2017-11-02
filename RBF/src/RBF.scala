import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.Variance
import scala.util.Random

object RBF {
//  1507690683580l//
  val seed = 1507690683580l
  println(seed)
  val rand =  new java.util.Random(seed)
  val sampleSize = 75
  val totalEpochs = 100
  var bases = Array(2,4,7,11,16)
  val learningRates = Array(0.01, 0.02)
  var inputs = new Array[Double](sampleSize)
  var desiredOutputs = new Array[Double](sampleSize)
//  var dataPoints: Map[Double, Double] = null

  def joinHosts(hosts: Seq[String], port: String): String = {
    val joined = new StringBuilder()
    hosts.zipWithIndex.foreach({case(host, i) =>{
      var hostPort = (s"$host:$port")
      if (i < hosts.size -1) {
        hostPort = hostPort.concat(",")
      }
      joined.append(hostPort)
    }})
    joined.toString()
  }

  def main(args: Array[String]): Unit = {

//    val hosts = Seq("storage01", "storage02", "storage03")
//    val port = "9092"
//    println(joinHosts(hosts, port))

    import java.util.StringTokenizer
    val property = System.getProperty("java.library.path")
    val parser = new StringTokenizer(property, ";")
    while ( {
      parser.hasMoreTokens
    }) println(parser.nextToken)

//    generateData()
//    for(rate <- learningRates){
//      bases.foreach(base => {
//        println(s"********* RUNNING with Bases: $base, LearningRate: $rate, SameVariance: false")
//        runTestWith(base, rate, false)
//      })
//    }
//
//    for(rate <- learningRates){
//      bases.foreach(base => {
//        println(s"********* RUNNING with Bases: $base, LearningRate: $rate, SameVariance: true")
//        runTestWith(base, rate, true)
//      })
//    }
//
////    leastMeanSquare(kMeans(11, false),0.01, 11 )
  }

  def runTestWith(base: Int, learningRate: Double, useSameVariance: Boolean): Unit ={
    val gaussians = kMeans(base, useSameVariance)
    val yeilds = leastMeanSquare(gaussians, learningRate, base)
    RMSerror(yeilds)

  }

  def RMSerror(yeilds: Array[Double]): Unit ={
    var errorSqaureSum = 0.0
    desiredOutputs.zip(yeilds).foreach({case (desired, yeilds) => {
      errorSqaureSum += Math.pow(desired - yeilds, 2)
    }})

    val RMS = Math.sqrt(errorSqaureSum/sampleSize)
    println(s"RMS: $RMS")

  }

  def euclideanNormSquared(i : Double, j: Double): Double ={
    Math.pow(euclideanNorm(i, j), 2)
  }

  def euclideanNorm(i : Double, j: Double): Double ={
    Math.abs(i - j)
  }

  def generateData() ={

//    dataPoints =
      (0 until sampleSize).foreach(i => {
      val x = threeDP(rand.nextDouble)
      val noise = -0.1 + (0.1 - (-0.1)) * rand.nextDouble
      val h = 0.5 + (0.4 * Math.sin(2 * Math.PI * x)) + noise
      inputs(i) = x
      desiredOutputs(i) = threeDP(h)
//      (x , threeDP(h))
    })
  }

  def threeDP(num: Double): Double ={
    Math.round(num * 1000.0) / 1000.0
  }

  def kMeans(base: Int, useSameVariance: Boolean): mutable.LinkedHashMap[Double,Double] ={//Array[(Double) => Double] ={
    Random.setSeed(seed)

    var initialMeans = Random.shuffle(inputs.toList).takeRight(base)
//    println("Initial Means")
//    initialMeans.foreach(println(_))
    var clusters : mutable.LinkedHashMap[Double, ListBuffer[Double]] = null
    var meanChanged = false
    do{
//      println("Clustering")
      clusters = mutable.LinkedHashMap[Double, ListBuffer[Double]]()//.withDefaultValue(ListBuffer[(Double, Double)]())
      meanChanged = false
      inputs.foreach(point => {
        var currentMinimumCluster = (Double.NegativeInfinity, Double.PositiveInfinity)
        initialMeans.foreach(mean => {
          val norm = euclideanNormSquared(point, mean)
          if(norm < currentMinimumCluster._2 && mean != currentMinimumCluster._1){
            currentMinimumCluster = (mean, norm)
          }
        })
        var cluster = clusters.getOrElse(currentMinimumCluster._1, ListBuffer[Double]())
        clusters.put(currentMinimumCluster._1, cluster += point)
//        println(currentMinimumCluster._1, cluster)
      })

//      println("Updating means")
      clusters = clusters.map({case (mean, cluster) => {
        val clusterSum = cluster.foldLeft((0.0))({
          case (accA, a) => accA + a
        })
        val newMean = threeDP(clusterSum / cluster.size)
//        println(s"OLD: ($mean, $cluster)")
        if(newMean != mean){
          meanChanged = true
        }
//        println(s"NEW: ($newMean, $cluster)")
        (newMean, cluster)
      }

      })
      initialMeans = clusters.keySet.toList

    }while(meanChanged)


//    clusters.foreach(println)
//    print("All Clusters with > 1 elements: ")
    val i = 0
    var sizeOneClusters = mutable.ListBuffer[Int]()
    clusters.zipWithIndex.foreach({
      case ((mean, cluster), i) =>
        if(cluster.size <= 1 ){
          sizeOneClusters += i
        }

    })

    var meanVariance : mutable.LinkedHashMap[Double, Double]= null
    if(useSameVariance){
      val defaultVariance = Math.pow(maxClusterDistance(clusters.keysIterator) / Math.sqrt(2 * base), 2)
//      println(defaultVariance)
      meanVariance = clusters.map(tuple => (tuple._1, defaultVariance))
    }else {
      meanVariance = clusters.map({ case (mean, cluster) => {
        val variance = getVariance(cluster.filter(data => data.isNaN == false))

        (mean, variance)
      }
      })
//      meanVariance.foreach(println)
      val averageVariance = meanVariance.map(_._2).filter(!_.isNaN).sum / meanVariance.size
      meanVariance = meanVariance.map({case(mean, variance) => {
        if (variance.isNaN){
          (mean, averageVariance)
        }else{
          (mean, variance)
        }

      }})
//      meanVariance.foreach(println)
    }

//    val gaussians =  new Array[(Double) => Double](base + 1)
//
//      meanVariance.zipWithIndex.foreach({ case ((mean, variance), i) => {
//        gaussians(i) = gaussian(_: Double, mean, variance)
//      }
//      })
//
//    gaussians(gaussians.length-1) =  gaussian(_: Double, 0.0, 0.0)
//    gaussians.zipWithIndex.foreach({case(g,i) => if(g == null){
//      println(s"g: $i is null")
//    }})
//    return gaussians
    meanVariance
  }

//  def leastMeanSquare(gaussians: Array[(Double) => Double], learningRate : Double, base: Int ): Array[Double] ={
    def leastMeanSquare(meanVariance: mutable.LinkedHashMap[Double,Double], learningRate : Double, base: Int ): Array[Double] ={
    var currentEpoch = 0
    var weights = new Array[Double](base + 1)
    //include bias
    (0 to base ).foreach(i => weights(i) = threeDP(rand.nextDouble()))

    var yeilds = new Array[Double](inputs.length)
      meanVariance+=((1.0,1.0))

    while(currentEpoch < totalEpochs) {
//      println(s"######## EPOCH $currentEpoch###########")
//      weights.foreach(weight => println(twoDP(weight)))
      inputs.zipWithIndex.foreach({ case (point, i) => {

        val yeild = meanVariance.zip(weights).zipWithIndex.map({
          case ((tuple, weight), i) =>
            if(i == weights.length - 1){
              1 * weight
            }else{
              gaussian(point, tuple._1, tuple._2) * weight
            }
            })
          .sum
        yeilds(i) = threeDP(yeild)
        weights = weights.map({ case (weight) => {
          weight + (learningRate * (desiredOutputs(i) - yeild) * point)
        }
        })
      }
      })
      currentEpoch+=1
    }
//    println("Input, Desired, Yeild")
//    desiredOutputs.zip(yeilds).zip(inputs).foreach({case ((desired, yeild), input) => println(s"$input, $desired, $yeild")})
    print("Inputs,   ")
    inputs.foreach(inputs => print(s"$inputs, "))
    println
    print("Desired,  ")
    desiredOutputs.foreach(desired => print(s"$desired, "))
    println
    print("Yeild,    ")
    yeilds.foreach(yeild => print(s"$yeild, "))
    println

    return yeilds


  }

  def getMean(data: ListBuffer[Double]): Double = {
    var sum = 0.0
    for (a <- data) {
      sum += a
    }
    sum / data.size
  }

  def getVariance(data: ListBuffer[Double]): Double = {
    val mean = getMean(data)
    var temp = 0.0
    for (a <- data) {
      temp += (a - mean) * (a - mean)
    }
    temp / (data.size - 1)
  }

  def gaussian(input: Double, center: Double, variance : Double): Double ={
//    Math.exp(   (-1/2*variance) * euclideanNormSquared(input, center))
    if(center == 0.0 || variance == 0.0){
      return 1.0
    }
    var gaussian = Math.exp(   (- 0.5 * euclideanNormSquared(input, center))  /variance)

//    val newGaussian =  pdf(input, center, Math.sqrt(variance))
//    val get = getY(input, center, variance)
    gaussian
//    get
//    newGaussian
  }

  def getY(x: Double, mean: Double, variance: Double) : Double = {
    val stdDeviation = Math.sqrt(variance)
    Math.pow(Math.exp(-(((x - mean) * (x - mean)) / ((2 * variance)))), 1 / (stdDeviation * Math.sqrt(2 * Math.PI)))

  }

  // return pdf(x) = standard Gaussian pdf
  def pdf(x: Double): Double = Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI)

  // return pdf(x, mu, signma) = Gaussian pdf with mean mu and stddev sigma
  def pdf(x: Double, mu: Double, sigma: Double): Double = pdf((x - mu) / sigma) / sigma

  def maxClusterDistance (clusters: Iterator[Double]): Double ={
    var maxClusterDistance = Double.NegativeInfinity
    var (iter1, iter2) = clusters.duplicate
    iter1.foreach(outer => {
      iter2.foreach(inner => {
        val norm = euclideanNorm(inner, outer)
//        println(s"$norm - $inner - $outer")
        if(norm > maxClusterDistance){
          maxClusterDistance = norm
        }
      })
    })
//    println(s"Max Cluster distance: $maxClusterDistance")
    maxClusterDistance
  }


}
