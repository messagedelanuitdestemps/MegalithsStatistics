package org.messagedelanuitdestemps.anglestest

import scala.io.Source
import Math._

class CollectionGpsPoints(csvFile: String) {
   // val gpsPoints: List[GpsPoint] = Source.fromFile(csvFile).getLines().toList.map {e => new GpsPoint(e)}
    val gpsPoints: List[GpsPoint] = csvFile.split("\n").toList.map {e => new GpsPoint(e)}


	val listeAngleRemarquables = List(0.0, 90.0, 45.0, 26.56, 18.43, 14.04, 11.31, 9.46, 8.13, 7.12, 6.34, 33.69, 30.96, 38.66, 35.54, 36.87, 22.62, 16.26)

    /*def iterateDistanceOnAllPoints = {
        val indexedPoints = this.gpsPoints.zipWithIndex
        indexedPoints.filter { case(e1, i1) => i1 < indexedPoints.length - 1}.map {case (e1, i1) => {
            indexedPoints.filter  { case (e2, i2) => i2 > i1 }.map { case (e2, i2) => println(e1.name.concat(" - ".concat(e2.name.concat(" : ".concat(e1.simpleDistance(e2).toString))))) }
        }}
    }

    def iterateOnAllUnorderedCoupleOfPoints(function: (GpsPoint, GpsPoint) => Double) = {
        val indexedPoints = this.gpsPoints.zipWithIndex
        indexedPoints.filter { case(e1, i1) => i1 < indexedPoints.length - 1}.map {case (e1, i1) => {
            indexedPoints.filter  { case (e2, i2) => i2 > i1 }.map { case (e2, i2) => println(e1.name.concat(" - ".concat(e2.name.concat(" : ".concat(function(e1, e2).toString))))) }
        }}
    }

    def iterateOnAllOrderedCoupleOfPoints(function: (GpsPoint, GpsPoint) => Double) = {
        val indexedPoints = this.gpsPoints.zipWithIndex
        indexedPoints.map {case (e1, i1) => {
            indexedPoints.filter  { case (e2, i2) => i2 != i1 }.map { case (e2, i2) => println(e1.name.concat(" - ".concat(e2.name.concat(" : ".concat(function(e1, e2).toString))))) }
        }}
    }*/

	def calcNombreAnglesRemarquables( precision :  Double) : Int = {
	   def precisionTo(val1 : Double, val2 : Double, precision : Double) : Boolean = {
			(val1 - val2).abs < precision
	   }
	   def testAngles(ang : Double, precision : Double) : Boolean = {
			(precisionTo(ang,0.0,precision) || precisionTo(ang,90.0,precision) || precisionTo(ang,45.0,precision) || 
			 precisionTo(ang,26.56,precision) || precisionTo(ang, 18.43,precision)  ||  precisionTo(ang,14.04,precision) || 
			 precisionTo(ang,11.31,precision) ||
			 precisionTo(ang,9.46,precision) || precisionTo(ang,8.13,precision) || precisionTo(ang,7.12,precision) || 
			 precisionTo(ang,6.34,precision) || precisionTo(ang,33.69,precision) || precisionTo(ang,30.96,precision) || 
			 precisionTo(ang,38.66,precision) || precisionTo(ang,35.54,precision) ||
			 precisionTo(ang,36.87,precision) || precisionTo(ang,22.62,precision) || precisionTo(ang,16.26,precision) )
		}
	   val iPoints = gpsPoints.zipWithIndex
	   (for {i <- iPoints; j <- iPoints if i._2 < j._2} yield List(testAngles(i._1.simpleAngle(j._1), precision), testAngles(j._1.simpleAngle(i._1), precision)))
		.flatten.filter{ e => e }.size
      }

// private[this] 

	def calcProba(nbrDoubletMesureAngleRemarquables : Int, nbmglt : Int, nbAnglRmq : Int, precision : Double) : Double = {
		def combinatoire ( n : BigDecimal, p : BigDecimal) : BigDecimal= {
			println("Cn=%f p=%f".format(n,p))
			def fact(n : BigDecimal) : BigDecimal = { 
					var result : BigDecimal = 1;
					for (i  <- 1 to n.toInt) {
						   result *= i;
    					}
    					result
			}
			fact(n)/(fact(n-p)*fact(p))
		}
		val nbm = (nbmglt*nbmglt - nbmglt).toDouble/2
		val fp2p1 = nbAnglRmq*precision
		val fp2  = (fp2p1/(45+precision))*(2 - fp2p1/(45+precision))
		val probs = (0 to nbrDoubletMesureAngleRemarquables).map { i : Int => 
			val res = combinatoire(nbm.toLong, i) * scala.math.pow(fp2, i.toDouble) * scala.math.pow(1.0 - fp2, (nbm - i))
			//println(res)
			res
		}.sum
		1 - probs.toDouble
	}

     def calcProbaThis(precision : Double) : Double = {
		calcProba(calcNombreAnglesRemarquables(precision), gpsPoints.size, listeAngleRemarquables.size, precision)
	}

}

/*val collection = new CollectionGpsPoints("points.csv")

println("First try :")
collection.iterateDistanceOnAllPoints

println("Distances : ")
collection.iterateOnAllUnorderedCoupleOfPoints(collection.simpleDistance)

println("Angles : ")
collection.iterateOnAllOrderedCoupleOfPoints(collection.simpleAngle)
*/
