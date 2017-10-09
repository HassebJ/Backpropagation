import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object RBF {
  val seed = 8
  val rand =  new java.util.Random(seed)
  val sampleSize = 75
  val totalEpochs = 10000
  val k = 10//sampleSize/2
  var inputs = Array[Double](sampleSize)
  var desiredOutputs = Array[Double](sampleSize)
  var dataPoints: Map[Double, Double] = null

  var gaussians =  new Array[(Double) => Double](k + 1)

  def main(args: Array[String]): Unit = {

//    generateData()
    kMeans()
    leastMeanSquare(0.02)
  }

  def euclideanNormSquared(i : Double, j: Double): Double ={
    Math.pow(euclideanNorm(i, j), 2)
  }

  def euclideanNorm(i : Double, j: Double): Double ={
    Math.abs(i - j)
  }

  def generateData() ={

    dataPoints = (1 to sampleSize).map(i => {
      val x = twoDP(rand.nextDouble)
      val noise = -0.1 + (0.1 - (-0.1)) * rand.nextDouble
      val h = 0.5 + 0.4 * Math.sin(2 * Math.PI * x) + noise
      (x , twoDP(h))
    }).toMap

//    dataPoints

    inputs = dataPoints.map(points => {
      points._1
    }).toArray

    desiredOutputs = dataPoints.map(points =>{
      points._2
    }).toArray

//    dataPoints.foreach(println)
//    println("######################")
//    inputs.zip(desiredOutputs).foreach({case (x, y) => println(s"($x, $y)")})

//    dataPoints(0)
//    return dataPoints
//    println("DataPoints")
//    dataPoints.foreach(println)
//    println(dataPoints.length)
//    dataPoints
  }

  def twoDP(num: Double): Double ={
    Math.round(num * 100.0) / 100.0
  }

  def kMeans(): Unit ={
    Random.setSeed(seed)
    generateData()

    var initialMeans = Random.shuffle(inputs.toList).takeRight(k)
    println("Initial Means")
    initialMeans.foreach(println(_))
    var clusters : mutable.Map[Double, ListBuffer[Double]] = null
    var meanChanged = false
    do{
//      println("Clustering")
      clusters = mutable.HashMap[Double, ListBuffer[Double]]()//.withDefaultValue(ListBuffer[(Double, Double)]())
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
        val newMean = twoDP(clusterSum / cluster.size)
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


    clusters.foreach(println)
    print("Cluster with just one element: ")
    println(clusters.forall({case (mean, cluster) => cluster.size > 1 }))

    val meanVariance = clusters.map({case (mean, cluster) => {
        val variance = cluster.foldLeft((0.0))({
          case (accA, a) => accA + euclideanNormSquared(a, mean)
        }) / cluster.size
        (mean, variance)
      }})

    meanVariance.foreach(println)
    val defaultVariance = Math.pow(maxClusterDistance(clusters.keysIterator) / Math.sqrt(2 * k), 2)
    println(defaultVariance)

    inputs.foreach(point => {
      meanVariance.zipWithIndex.foreach({ case ((mean, variance), i) => {
        gaussians(i) = gaussian(_: Double, mean, variance)
      }
      })
    })

    gaussians(gaussians.length-1) =  gaussian(_: Double, 0, 0)
  }

  def leastMeanSquare(learningRate : Double ): Unit ={
    var currentEpoch = 0
    var weights = new Array[Double](k + 1)
    //include bias
    (0 to k ).foreach(i => weights(i) = twoDP(rand.nextDouble()))
    var yeilds = new Array[Double](inputs.length)

    while(currentEpoch < totalEpochs) {
      println(s"######## EPOCH $currentEpoch###########")
      weights.foreach(weight => println(twoDP(weight)))
      inputs.zipWithIndex.foreach({ case (point, i) => {

        val yeild = gaussians.zip(weights).map({ case (partialGaussian, weight) => partialGaussian(point) * weight}).sum
        yeilds(i) = yeild
        weights = weights.map({ case (weight) => {
          weight + (learningRate * (desiredOutputs(i) - yeild) * point)
        }
        })
      }
      })
      currentEpoch+=1
    }

    println("Desired - Yeild")
    desiredOutputs.zip(yeilds).foreach({case (desired, yeild) => println(s"$desired - $yeild")})
  }

  def gaussian(input: Double, center: Double, variance : Double): Double ={
    Math.exp((-1/2*variance) * euclideanNormSquared(input, center))
  }

  def maxClusterDistance (clusters: Iterator[Double]): Double ={
    var maxClusterDistance = Double.NegativeInfinity
    var (iter1, iter2) = clusters.duplicate
    iter1.foreach(outer => {
      iter2.foreach(inner => {
        val norm = euclideanNorm(inner, outer)
        println(s"$norm - $inner - $outer")
        if(norm > maxClusterDistance){
          maxClusterDistance = norm
        }
      })
    })
    println(s"Max Cluster distance: $maxClusterDistance")
    maxClusterDistance
  }


}

class RBF {



}
