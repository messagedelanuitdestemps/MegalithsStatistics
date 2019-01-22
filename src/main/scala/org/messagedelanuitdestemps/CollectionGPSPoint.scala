package org.messagedelanuitdestemps.anglestest

import scala.io.Source
import scala.util.Random
import scala.math.random
import Math._

class CollectionGpsPoints(csvFile: String) {
   // val gpsPoints: List[GpsPoint] = Source.fromFile(csvFile).getLines().toList.map {e => new GpsPoint(e)}
    var gpsPoints: List[GpsPoint] = csvFile.split("\n").toList.map {e => val o = new GpsPoint(""); o.fromDecimalGPSPoint(e); o}


	val listeAngleRemarquables = List(0.0, 90.0, 45.0, 26.56, 18.43, 14.04, 11.31, 9.46, 8.13, 7.12, 6.34, 33.69, 30.96, 38.66, 35.54, 36.87, 22.62, 16.26)

	def allCombinations(n : Int) : Iterator[List[GpsPoint]] = {
		def combs[A](n: Int, l: List[A]): Iterator[List[A]] = n match {
			case _ if n < 0 || l.lengthCompare(n) < 0 => Iterator.empty
				case 0 => Iterator(List.empty)
				case n => l.tails.toList.par.flatMap({
						case Nil => Nil
						case x :: xs => combs(n - 1, xs).map(x :: _)
						}).toIterator
		}
		combs(n,gpsPoints)
	}


	private def precisionTo(val1 : Double, val2 : Double, precision : Double) : Boolean = {
			(val1 - val2).abs < precision
	}

	private def testAngles(ang : Double, precision : Double) : Boolean = {
			(precisionTo(ang,0.0,precision) || precisionTo(ang,90.0,precision) || precisionTo(ang,45.0,precision) || 
			 precisionTo(ang,26.56,precision) || precisionTo(ang, 18.43,precision)  ||  precisionTo(ang,14.04,precision) || 
			 precisionTo(ang,11.31,precision) ||
			 precisionTo(ang,9.46,precision) || precisionTo(ang,8.13,precision) || precisionTo(ang,7.12,precision) || 
			 precisionTo(ang,6.34,precision) || precisionTo(ang,33.69,precision) || precisionTo(ang,30.96,precision) || 
			 precisionTo(ang,38.66,precision) || precisionTo(ang,35.54,precision) ||
			 precisionTo(ang,36.87,precision) || precisionTo(ang,22.62,precision) || precisionTo(ang,16.26,precision) )
	}

	private def combinatoire ( n : BigDecimal, p : BigDecimal) : BigDecimal= {
			//println("Cn=%f p=%f".format(n,p))
			def fact(n : BigDecimal) : BigDecimal = { 
					var result : BigDecimal = 1;
					for (i  <- 1 to n.toInt) {
						   result *= i;
    					}
    					result
			}
			fact(n)/(fact(n-p)*fact(p))
	}


	def monteCarloAllCombs(tailleTab : Int, tailleMaxNbrSousListes : Int, precision : Double) : List[Int] = {
		val agps = gpsPoints.toArray
		val rand = new Random(random.toInt)
		def construireUneListeTaille(taille : Int) : List[GpsPoint]= {
			val listeUniq = (1 to taille*5).map { i => rand.nextInt(gpsPoints.length) }.distinct.take(taille).toList
			listeUniq.map {i => agps.apply(i)}
		}

		(1 to tailleMaxNbrSousListes).par.map { _ =>
			calcNombreAnglesRemarquablesForListe(construireUneListeTaille(tailleTab), precision)
		}.toList
	}



   
	def calcNombreAnglesRemarquables( precision :  Double) : Int = { //TODO utiliser la plus générale
		val iPoints = gpsPoints.zipWithIndex
		(for {i <- iPoints; j <- iPoints if i._2 < j._2} yield List(testAngles(i._1.simpleAngle(j._1), precision), testAngles(j._1.simpleAngle(i._1), precision)))
		.flatten.filter{ e => e }.size
	}

	def calcNombreAnglesRemarquablesForListe( myList : List[GpsPoint], precision :  Double) : Int = {
		val iPoints = myList.zipWithIndex
		(for {i <- iPoints; j <- iPoints if i._2 < j._2} yield List(testAngles(i._1.simpleAngle(j._1), precision), testAngles(j._1.simpleAngle(i._1), precision)))
		.flatten.filter{ e => e }.size
	}



	def calcNombreAnglesRemarquablesForSubGroups( precision :  Double, taille : Int) : List[Int] = {
		val iPointsL =  allCombinations(taille).toList.map { l => l.zipWithIndex }
		iPointsL.map { iPoints => (for {i <- iPoints; j <- iPoints if i._2 < j._2} yield List(testAngles(i._1.simpleAngle(j._1), precision), testAngles(j._1.simpleAngle(i._1), precision)))
		.flatten.filter{ e => e }.size }

	}


      

// private[this] 

	def calcProba(nbrDoubletMesureAngleRemarquables : Int, nbmglt : Int, nbAnglRmq : Int, precision : Double) : Double = {
		val nbm = (nbmglt*nbmglt - nbmglt).toDouble/2
		val fp2p1 = nbAnglRmq*precision
		val fp2  = (fp2p1/(45+precision))*(2 - fp2p1/(45+precision))
		val probs = (0 to nbrDoubletMesureAngleRemarquables).par.map { i : Int => 
			val res = combinatoire(nbm.toLong, i) * scala.math.pow(fp2, i.toDouble) * scala.math.pow(1.0 - fp2, (nbm - i))
			//println(res)
			res
		}.sum
		1 - probs.toDouble
	}

     def calcProbaForAllThis(precision : Double) : Double = {
		calcProba(calcNombreAnglesRemarquables(precision), gpsPoints.size, listeAngleRemarquables.size, precision)
	}

    
     def calcProbaForSubGroupThis(precision : Double, taille : Int) : List[Double] = {
		calcNombreAnglesRemarquablesForSubGroups(precision,taille).par.map {
			subgrp =>
			 calcProba(subgrp, taille, listeAngleRemarquables.size, precision)
		}.toList
     }

     def calcProbaForSubGroupMonteCarlo(precision : Double, tailleEchantillon : Int, tailleMaxATester : Int) : List[Double] = {
		monteCarloAllCombs(tailleEchantillon,tailleMaxATester, precision).map {
			subgrp =>
			 calcProba(subgrp, tailleEchantillon, listeAngleRemarquables.size, precision)
		}
     }



	//TODO : Prendre des sous ensembles au hasard et construire le profil de proba de chacun d'entre eux pour cracher une sorte de courbe...

}

/*val collection = new CollectionGpsPoints("points.csv")

println("First try :")
collection.iterateDistanceOnAllPoints

println("Distances : ")
collection.iterateOnAllUnorderedCoupleOfPoints(collection.simpleDistance)

println("Angles : ")
collection.iterateOnAllOrderedCoupleOfPoints(collection.simpleAngle)
*/
