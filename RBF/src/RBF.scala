import scala.collection.mutable.ListBuffer
import scala.util.Random

object RBF {
  val seed = 8
  val rand =  new java.util.Random(seed)
  val sampleSize = 75
  val k = sampleSize/2
  def main(args: Array[String]): Unit = {

//    generateData()
    kMeans()
  }

  def euclideanNormSquared(i : (Double, Double), j: (Double, Double)): Double ={
    Math.pow(euclideanNorm(i, j), 2)
  }

  def euclideanNorm(i : (Double, Double), j: (Double, Double)): Double ={
    Math.sqrt(Math.pow(i._1 - j._1, 2) + Math.pow(i._2 - j._2, 2))
  }

  def generateData(): List[(Double,Double)] ={

    val dataPoints = (1 to sampleSize).map(i => {
      val x = twoDP(rand.nextDouble)
      val noise = -0.1 + (0.1 - (-0.1)) * rand.nextDouble
      val h = 0.5 + 0.4 * Math.sin(2 * Math.PI * x) + noise
      (x, twoDP(h))
    }).toList

//    dataPoints(0)

//    return dataPoints
    println("DataPoints")
    dataPoints.foreach(println)
//    println(dataPoints.length)
    dataPoints
  }

  def twoDP(num: Double): Double ={
    Math.round(num * 100.0) / 100.0
  }

  def kMeans(): Unit ={
    Random.setSeed(seed)
    val dataPoints = generateData()
//    var initialMeans = Set[(Double, Double)]()
//    while (initialMeans.size < k ){
//      initialMeans = initialMeans + dataPoints(rand.nextInt(dataPoints.size) )
//    }

    var initialMeans = Random.shuffle(dataPoints).takeRight(k)
    println("Initial Means")
    initialMeans.foreach(println(_))
    var clusters = collection.mutable.HashMap[(Double, Double), ListBuffer[(Double, Double)]]()//.withDefaultValue(ListBuffer[(Double, Double)]())


    var meanChanged = false
    do{
      meanChanged = false
      dataPoints.foreach(dataPoint => {
        var currentMinimumCluster = (initialMeans(0), Double.PositiveInfinity)
        initialMeans.foreach(mean => {
          val norm = euclideanNormSquared(dataPoint, mean)
          if(norm < currentMinimumCluster._2 && mean != currentMinimumCluster._1){
            currentMinimumCluster = (mean, norm)
          }
        })
        var cluster = clusters.getOrElse(currentMinimumCluster._1, ListBuffer[(Double, Double)]())
        clusters.put(currentMinimumCluster._1, cluster += dataPoint)
      })

      clusters = clusters.map({case (mean, cluster) => {
        val clusterSum = cluster.foldLeft((0.0, 0.0))({
          case ((accA, accB), (a, b)) => (accA + a, accB + b)
        })
        val newMean = (twoDP(clusterSum._1 / cluster.size), twoDP(clusterSum._2 / cluster.size))
//        println(s"NewMean: $newMean - Mean: $mean")
        if(newMean != mean){
          meanChanged = true
        }
        (newMean, cluster)
      }

      })
      initialMeans = clusters.keySet.toList

    }while(meanChanged)

  clusters.foreach(println)
    println(clusters.forall({case (mean, cluster) => cluster.size > 1 }))

    val meanVariance = clusters.map({case (mean, cluster) => {
        val variance = cluster.foldLeft((0.0, 0.0))({
          case ((accA, accB), (a, b)) => (accA + euclideanNormSquared((a, b), mean), accB + 1)
        })._1 / cluster.size
        (mean, variance)
      }})

    meanVariance.foreach(println)
    val defaulVariance = Math.pow(maxClusterDistance(clusters.keysIterator) / Math.sqrt(2 * k), 2)
    println(defaulVariance)
//    clusters.keysIterator.duplicate


    ////////////////LEAST MEAN SQUARE

    val initialWeights = (1 to k +1 ).map(i => twoDP(rand.nextDouble()))

    val bias = 0.5
    dataPoints.foreach(point => {
//      var gaussians = scala.collection.mutable.Map() ++ meanVariance.map({case (mean, variance) => {
//        gaussian(point, mean, variance)
//      }})
      var gaussians = meanVariance.map({case (mean, variance) =>
        gaussian(point, mean, variance)
      })
//      gaussians = gaussians += 1.0


//      gaussians + 1.0

      gaussians.zip(initialWeights)

      val yeild = gaussians.zip(initialWeights).map({case (input ,weight) => input * weight}).sum + bias

      val desired = 1
      val learningRate = 0.1
      val x = 8
      initialWeights.map(weight => weight + (learningRate * (desired - yeild) * x))

//      w(n)+η[d(n)− y(n)]x(n)

    })


//
  }

  def gaussian(input: (Double, Double), center: (Double, Double), variance : Double): Double ={
    Math.exp((-1/2*variance) * euclideanNormSquared(input, center))
  }

  def maxClusterDistance (clusters: Iterator[(Double, Double)]): Double ={
    var maxClusterDistance = Double.NegativeInfinity
    var (iter1, iter2) = clusters.duplicate
    iter1.foreach(outer => {
      iter2.foreach(inner => {
        val norm = euclideanNorm(inner, outer)
        if(norm > maxClusterDistance){
          maxClusterDistance = norm
        }

      })
    })
    maxClusterDistance
  }


}

class RBF {



}
