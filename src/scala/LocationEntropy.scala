import scala.collection.mutable.ListBuffer

object LocationEntropy {
  case class PointOfInterest(userID: String, poiID: Int, poiFreq: Int)

  def calculateEntropy(poiId: Int, poiObjList: List[PointOfInterest]) : Double = {
    val log2 = (x: Double) => math.log10(x)/math.log10(2.0) // log2 lambda

    // sum of all frequency
    val sumAllFreq: Int = poiObjList.collect{case p if p.poiID==poiId => p.poiFreq}.sum

    // calculate entropy and sum up
    poiObjList.filter(_.poiID == poiId)
      .foldLeft(0D) { (z: Double, x: PointOfInterest) => {
        val sumPoiFreq = poiObjList.collect { case p if p.userID == x.userID && p.poiID == poiId => p.poiFreq }.sum
        val prob = sumPoiFreq.toDouble / sumAllFreq.toDouble
        z + prob * log2(prob) * (-1)
      }}
  }

  def main(args: Array[String]) {
    val fileReader = io.Source.fromFile("src/resources/dataset/userVisits-ijcai15/userVisits-Toro.csv")
    var objCollection = new ListBuffer[PointOfInterest]()
    fileReader.getLines.drop(1).foreach(line => {
      val Array(_, userId, _, poiID, _, poiFreq, _) = line.split(";").map(_.trim)
      val poiObj = PointOfInterest(userId, poiID.toInt, poiFreq.toInt)
      objCollection += poiObj
    })
    fileReader.close

    // Calculate entropy for the first few elements - TEST
    val uniquePOIs = objCollection.map(_.poiID).distinct.sorted.reverse
    val endRange = if (uniquePOIs.length>10) 10 else uniquePOIs.length
    for (i <- 0 until endRange) {
      println(s"For POI: ${uniquePOIs(i)} -> Location Entropy: ${calculateEntropy(uniquePOIs(i), objCollection.toList)}")
    }
  }
}
