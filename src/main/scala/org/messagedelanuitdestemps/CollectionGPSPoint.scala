package org.messagedelanuitdestemps

import scala.io.Source
import scala.util.Random
import scala.math.random
import Math._
//import scalaz.Memo
import scala.collection.mutable._
//import scalaz._, Scalaz._
import scala.xml._
//import org.messagedelanuitdestemps.MyImplicits._


class CollectionGpsPoints(csvFile: String) {
   // val gpsPoints: List[GpsPoint] = Source.fromFile(csvFile).getLines().toList.map {e => new GpsPoint(e)}
    var gpsPoints: List[GpsPoint] = csvFile.split("\n").filter( _.size >0).map {e => val o = new GpsPoint(""); o.fromDecimalGPSPoint(e); o}.toList
    var gpsPointsBackup : List[GpsPoint] = List.empty
    var listeListLowProba : List[List[GpsPoint]] = List.empty
    var listeGpsPointPlusCites : List[(GpsPoint,Int)] = List.empty
    var isAngleRemarquableForCouple : Array[Boolean] = new Array[Boolean](pow(2,2*((log(gpsPoints.size.toDouble)/log(2)).toInt+1)).toInt)


    
		

		//new Array[Boolean](pow(2,2*((log(gpsPoints.size.toDouble)/log(2)).toInt+1)).toInt)



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


	private var memoFact : ArrayBuffer[BigDecimal] = new ArrayBuffer[BigDecimal](60000)

	def fact(n : BigDecimal) : BigDecimal =   { 
		var result : BigDecimal = 1
	// memoization à l'ancienne
		if (memoFact(n.toInt) > 0) memoFact(n.toInt) else {
			println("calcul")
			for (i  <- 1 to n.toInt) {
				result *= i;
				memoFact(i) = result;
			}
			memoFact(n.toInt) = result
			result
		}
	}
	// Init
	(1 to 60000).foreach { _ => memoFact += BigDecimal(0)}
	fact(5000)


	private def combinatoire ( n : BigDecimal, p : BigDecimal) : BigDecimal= {
		//println("Cn=%f p=%f".format(n,p))

		fact(n)/(fact(n-p)*fact(p))
	}



	def monteCarloAllCombs(tailleTab : Int, maxNbrSousListes : Int, precision : Double) : List[Double] = {
		val agps = gpsPoints.toArray
		val rand = new Random((random*random*78668787).toInt)
		val nbit = (log(gpsPoints.size.toDouble)/log(2)).toInt+1

		def construireUneListeTaille(taille : Int) : List[GpsPoint]= {
			val listeUniq = (1 to taille*5).map { i => rand.nextInt(gpsPoints.length) }.distinct.take(taille).toList
			listeUniq.map {i => agps.apply(i)}
		}


		val tailleListeAngleRemarquables = listeAngleRemarquables.size

		(1 to maxNbrSousListes).par.map { i =>
				//if ((random*random*78668787).toInt % 4567 == 0)
				val p = ((100.0/maxNbrSousListes)*i*10000).toInt
			if (p> 100000 && p % 100000 == 0) println(p/10000+"% effectué (modulo parallelisme)")

			val megListe = construireUneListeTaille(tailleTab)
			//TODO: on prend gpsPoints.toArray, et on construit le Array[Boolean] de taille pow(2,2*((log(gpsPoints.size.toDouble)/log(2)).toInt+1))
			//TODO: On fusionne construireUneListeTaille et calcNombreAnglesRemarquablesForListe, et pour chaque i,j, il calcul
			//TODO: makeFactor(i,j,gpsPoints.size), et va chercher dans le Array[Boolean] si cet angle est remarquable ou non. Si oui, il le compte
			//val resProba = calcProba(calcNombreAnglesRemarquablesForListe(megListe, precision),tailleTab,tailleListeAngleRemarquables,precision)
			val resProba = calcProba(makeGpsPointRandomListAndCountMatchingAngles(tailleTab,nbit,precision),tailleTab,tailleListeAngleRemarquables,precision)
	
			//val test = makeGpsPointRandomListAndCountMatchingAngles(tailleTab,nbit,precision)//TODO
			if (resProba < pow(10,-8)) listeListLowProba = listeListLowProba :+ megListe // TODO : les stocker et calculer les éventuelles intersections
								      // TODO : ensuite comparer la taille des intersections avec le randomized
								      // Logiquement, s'il y a intentionalité de placement des mégalithes, le randomized devrait pas être stable (quoique ?)
			resProba
		}.toList
	}



   
	def calcNombreAnglesRemarquables( precision :  Double) : Int = { //TODO utiliser la plus générale
		val iPoints = gpsPoints.zipWithIndex
		(for {i <- iPoints; j <- iPoints if i._2 < j._2} yield List(testAngles(i._1.simpleAngle(j._1), precision), testAngles(j._1.simpleAngle(i._1), precision)))
		.flatten.filter{ e => e }.size
	}

	def makeGpsPointRandomListAndCountMatchingAngles(taille : Int, nbit : Int, precision : Double) : Int = {
		// On calcul la liste
		val rand = new Random((random*random*76734587).toInt)
		val listeUniq = (1 to taille*5).map { i => rand.nextInt(gpsPoints.length) }.distinct.take(taille).toList
		val iPoints = listeUniq.zipWithIndex
		var countAnglesOk = 0
		//println("NEW")
		//println(nbit)
		for {i <- iPoints; j <- iPoints if i._2 < j._2} yield {//TODO : voir si on peut pas éviter le zipWithIndex
			//TODO : gérer le fait qu'on essaye les angles dans DEUX sens
			//println((i._1 << nbit) + j._1)
			//println((j._1 << nbit) + i._1)
			/*println("%s ; %s :::===> %b && %b".format(agps( i._1 ).toString,
								  agps( j._1  ).toString,
								  isAngleRemarquableForCouple( (i._1 << nbit) + j._1  ),
								  isAngleRemarquableForCouple( (j._1<< nbit) + i._1  )  ))*/
			countAnglesOk = countAnglesOk + (if (isAngleRemarquableForCouple( (i._1 << nbit) + j._1  )) 1 else 0) + (if (isAngleRemarquableForCouple( (j._1 << nbit) + i._1  )) 1 else 0)
		}

		//println("OLD")
		/*val listePourTest = listeUniq.map {i => agps.apply(i)}
		val iPointsTest = listePourTest.zipWithIndex
		var countAnglesOkTest = 0
		for {i <- iPointsTest; j <- iPointsTest if i._2 < j._2} yield {

			countAnglesOkTest = countAnglesOkTest + (if (testAngles(i._1.simpleAngle(j._1), precision)) 1 else 0) + (if (testAngles(j._1.simpleAngle(i._1), precision)) 1 else 0)
		}
		println("Ancienne méthode : %d, nouvelle méthode : %d\n".format(countAnglesOkTest,countAnglesOk))*/
		countAnglesOk		
	}

	def calcNombreAnglesRemarquablesForListe( myList : List[GpsPoint], precision :  Double) : Int = {
		val iPoints = myList.zipWithIndex
		var countAnglesOk = 0
		for {i <- iPoints; j <- iPoints if i._2 < j._2} yield {
			countAnglesOk = countAnglesOk + (if (testAngles(i._1.simpleAngle(j._1), precision)) 1 else 0)
			countAnglesOk = countAnglesOk + (if (testAngles(j._1.simpleAngle(i._1), precision)) 1 else 0)
		}
		countAnglesOk
	}



	def calcNombreAnglesRemarquablesForSubGroups( precision :  Double, taille : Int) : List[Int] = {
		val iPointsL =  allCombinations(taille).toList.map { l => l.zipWithIndex }
		iPointsL.map { iPoints => (for {i <- iPoints; j <- iPoints if i._2 < j._2} yield List(testAngles(i._1.simpleAngle(j._1), precision), testAngles(j._1.simpleAngle(i._1), precision)))
		.flatten.filter{ e => e }.size }

	}

	def genereJSONAngleRemarquables(precision : Double) : String = {
		val iPointsL =  allCombinations(2).toList
		val selection = iPointsL.filter { case l => val a = l.apply(0); val b = l.apply(1); testAngles(a.simpleAngle(b), precision) ||  testAngles(b.simpleAngle(a), precision) }.
				map {case l => (l.apply(0),l.apply(1))}
		"const lines = [" + selection.map { case (a,b) => "[[%f,%f],[%f,%f]]".formatLocal(java.util.Locale.US,a.latitude,a.longitude,b.latitude,b.longitude) }.mkString(",")+ "];\n"
	}

      

// private[this] 

	def calcProba(nbrDoubletMesureAngleRemarquables : Int, nbmglt : Int, nbAnglRmq : Int, precision : Double) : Double = {
		var sum : BigDecimal = 0
		val nbm = (nbmglt*nbmglt - nbmglt).toDouble/2
		val fp2p1 = nbAnglRmq*precision
		val fp2  = (fp2p1/(45+precision))*(2 - fp2p1/(45+precision))
		val probs = (0 to nbrDoubletMesureAngleRemarquables).foreach { i : Int => 
			val res = combinatoire(nbm.toLong, i) * scala.math.pow(fp2, i.toDouble) * scala.math.pow(1.0 - fp2, (nbm - i))
			//println(res)
			sum = sum + res
		}
		1 - sum.toDouble
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

    /* def calcProbaForSubGroupMonteCarlo(precision : Double, tailleEchantillon : Int, tailleMaxATester : Int) : List[Double] = {
		val tailleListeAngleRemarquables = listeAngleRemarquables.size
		monteCarloAllCombs(tailleEchantillon,tailleMaxATester, precision).map {
			nbrAnglRmqGrp =>
			 calcProba(nbrAnglRmqGrp, tailleEchantillon, tailleListeAngleRemarquables, precision)
		}
     }*/

     def genereRandomizedCollection(taille : Int) = {
	     //println("%s %f N %f".format("toto", random*4 +42 , random*4 - 2 ))
	     this.gpsPoints =  (1 to taille).map { i =>  "%s, %f, %f".formatLocal(java.util.Locale.US,"toto", random + 42 , random - 0.5 ).toString }.toList.mkString("\n")
		 .split("\n").toList.map {e => val o = new GpsPoint(""); o.fromDecimalGPSPoint(e); o}
     }

     def randomizedThisCollection(minDeg : Double, maxDeg : Double) = {
	this.gpsPointsBackup = this.gpsPoints
	this.gpsPoints = this.gpsPoints.map {e => e.randomizePosition(minDeg,maxDeg); e }
     }



     override def clone : CollectionGpsPoints = {
	     val o = new CollectionGpsPoints("")
		     o.gpsPoints = this.gpsPoints
		     o
     }

    def updateMeilleur(lis : List[(GpsPoint,Int)]) = {


	    def rightJoinOn[A,B]( l1 : List[A], l2 : List[B], pred : (A,B) => Boolean ) : List[(Option[A],B)] = {
		    if (l1.size == 0 && l2.size > 0)
			    l2.map { e => (None,e) }
		    else {
			    val res1 = for { b <- l2; a <- l1  } yield if (pred(a,b)) { (Some(a),b) } else {(None,b)}
			    res1.flatMap { case (a,b) => if (! a.isDefined && res1.exists{ case (c,d) =>  b == d && c.isDefined} ) { None } else Some((a,b)) }.distinct
                  }
                }
	

	def joinMeilleurs( a : (GpsPoint,Int), b : (GpsPoint,Int)) : Boolean = { a._1 == b._1 }
	if (listeGpsPointPlusCites.size == 0)
		this.listeGpsPointPlusCites = lis
	else {
		this.listeGpsPointPlusCites = rightJoinOn(listeGpsPointPlusCites,lis, joinMeilleurs ).map{case (Some((g1,n1)),(g2,n2))  => (g1,n1+n2) 
													  case (None, (g2,n2)  ) => (g2, n2) 
												  	  }		
	}
    }

    def compareProfilAvecGroupeControle(xmlsrc : String, precision : Double, tailleEchantillon : Int, tailleMaxATester : Int, minDegRandomization : Double, maxDeg : Double) : List[(Double, Double,Double)] = {
		// 1. on lance le Monte-Carlo
		val testMegalithOk = this.monteCarloAllCombs( tailleEchantillon, tailleMaxATester, precision)
		//println(this.listeListLowProba.reduce{ (a,b) => a.toSeq.intersect(b.toSeq).toList})
		val meilleur = this.listeListLowProba.flatten.distinct.map { e=> (e,this.listeListLowProba.flatten.count( _ == e)) }.sortBy { case (a,b) => b}.reverse
		this.listeGpsPointPlusCites = meilleur
		println(meilleur.take(15))

		// 2. On génère un nouvel objet CollectionGpsPoints de la même taille, on lance le Monte-Carlo

		val grpControl = new CollectionGpsPoints(loadGpx(xmlsrc))
		//grpControl.gpsPoints = this.gpsPoints.to.toList
		grpControl.randomizedThisCollection(minDegRandomization,maxDeg) //genereRandomizedCollection(this.gpsPoints.size)
		grpControl.precomputeAngleTest(precision)
		val testGroupeControl = grpControl.monteCarloAllCombs( tailleEchantillon, tailleMaxATester, precision)
		//this.gpsPoints = grpControl.gpsPointsBackup // Le clone a l'air de déconner, par ref ?
		val gmeilleur = grpControl.listeListLowProba.flatten.distinct.map { e=> (e,grpControl.listeListLowProba.flatten.count( _ == e)) }.sortBy { case (a,b) => b}.reverse
		grpControl.listeGpsPointPlusCites = gmeilleur
		println(gmeilleur)

		println("")
		// 3. On batit un petit histogramme.
		val buglst = (1 to 11).toList
		println(buglst)
		val basepcent = 100.0/tailleMaxATester
		buglst.map{ i : Int =>  
				val exposant = pow(10,-i.toDouble)
				println("Groupe testé vs groupe de contrôle")
				println("%.10f :   %d vs        %d ".format(exposant, testMegalithOk.filter( _ < exposant).size, testGroupeControl.filter( _ < exposant).size))
				(exposant, (basepcent*testMegalithOk.filter( _ < exposant).size), (basepcent*testGroupeControl.filter( _ < exposant).size))
			     }
    }

    def loadGpx(filename : String) : String =  (XML.loadFile(filename) \ "wpt").map { e => ( "%s, %s, %s".format( (e \("cmt")).text,  e \@("lat"),  e \@("lon")))}.mkString("\n")
    

    def precomputeAngleTest(precision : Double) = {
	val nbit = (log(this.gpsPoints.size.toDouble)/log(2)).toInt+1
	val iPoints = this.gpsPoints.toArray
 	var i: Int = 0
   	var j: Int = 0
	i = 0
	//println(isAngleRemarquableForCouple.size)
	while (i < iPoints.size) {
		j = 0
		while (j < i) {
			this.isAngleRemarquableForCouple((i << nbit) + j) = /*(iPoints(i),iPoints(j),*/testAngles(iPoints(i).simpleAngle(iPoints(j)), precision)
			this.isAngleRemarquableForCouple((j << nbit) + i) = /*(iPoints(j),iPoints(i),*/testAngles(iPoints(j).simpleAngle(iPoints(i)), precision)

			/*println("%s(%d) et %s(%d) ont pour la valeurs : %b && %b".format(iPoints(i).toString,(i << nbit) + j, iPoints(j).toString, (j << nbit) + i, 
									 this.isAngleRemarquableForCouple((i << nbit) + j),
									 this.isAngleRemarquableForCouple((j << nbit) + i) ));*/ { j += 1; j - 1 }
		}
		{ i += 1; i - 1 }
	}

    }

    def runNProfiles(nbRun : Int,xmlsrc : String, precision : Double, tailleEchantillon : Int, tailleMaxATester : Int, minDegRandomization : Double, maxDeg : Double) : List[(Double, Double,Double)] = {
	
	precomputeAngleTest(precision)
	/*var j : Int = 0

	for (i <- 0 until iPoints.size) {
		j = 0
		while (j < i) {
			isAngleRemarquableForCouple((i << nbit) + j) = testAngles(iPoints(i).simpleAngle(iPoints(j)), precision)
			isAngleRemarquableForCouple((j << nbit) + i) = testAngles(iPoints(j).simpleAngle(iPoints(i)), precision)
			println("Indices %d et %d ont pour la valeurs : %b && %b".format((i << nbit) + j,(j << nbit) + i, isAngleRemarquableForCouple((i << nbit) + j),
									 isAngleRemarquableForCouple((j << nbit) + i) )
			)
			j += 1
		}
	} */




	val ress : List[List[(Double, Double,Double)]] = (1 to nbRun).map { i =>
		println("Run n°%d".format(i))
		//this.gpsPoints = (new CollectionGpsPoints(loadGpx(xmlsrc))).gpsPoints
		this.compareProfilAvecGroupeControle(xmlsrc,precision,tailleEchantillon,tailleMaxATester,minDegRandomization, maxDeg)
		//List((1.0,2.0,3.0))
	}.toList
	println(this.listeGpsPointPlusCites.take(28))

	(0 to 10).map { i => // Pour chaque coef
	 val ec = (ress.map { e => e.apply(i)._2 }.sum)/nbRun
	 val gc = (ress.map { e => e.apply(i)._3 }.sum)/nbRun
	 (i.toDouble,ec,gc)
	}.toList
    }


    def genereXmlMeilleur(nb : Int) : String = {
	val l = this.listeGpsPointPlusCites.take(nb).map { case (g,n) => "<wpt lat=\"%f\" lon=\"%f\"><cmt>%s</cmt></wpt>".formatLocal(java.util.Locale.US,g.latitude,g.longitude,g.name)}
	"<gpx>"+ l.mkString("\n")+"</gpx>"	
    }


	//TODO : Prendre des sous ensembles au hasard et construire le profil de proba de chacun d'entre eux pour cracher une sorte de courbe...

}


