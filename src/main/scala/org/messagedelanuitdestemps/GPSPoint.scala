package org.messagedelanuitdestemps.anglestest


import math._
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.HashMap


class GpsPoint(csvLine: String) extends Cloneable {
    var position: (Double, Double, String) = (0.0,0.0,"")//parseCsvLine(csvLine)
    var latitude: Double = position._1
    var longitude: Double = position._2
    var name: String = position._3
	val EarthMeanRadius = 6367.532
	val Pi = 3.141592653589793



	private[this] def toCartesianVect(lat1rp : Double, lon1rp : Double) = {
			Vector3(EarthMeanRadius*sin(Pi/2 - lat1rp)*cos(lon1rp), EarthMeanRadius*sin(Pi/2 - lat1rp)*sin(lon1rp), EarthMeanRadius*cos(Pi/2 - lat1rp))
	}

	private[this] def getNormLNS(vec : Vector3) = {
		Vector3(vec.y,-vec.x,0.0)
	}

	/*private[this] def calcNormLoc( lat1r : Double, lon1r : Double, lat2r : Double, lon2r : Double) = {
		val cart1 = toCartesianVect(lat1r, lon1r)
		val cart2 = toCartesianVect(lat2r, lon2r)
		val x = cart1.y*cart2.z-cart2.y*cart1.z
		val y = -cart1.x*cart2.z+cart2.x*cart1.z
		val z = cart1.x*cart2.y-cart2.x*cart1.y
		Vector3(x,y,z)	
	}*/

	private[this] def calcNormLoc( cart1 : Vector3, cart2 : Vector3) = {
		val x = cart1.y*cart2.z-cart2.y*cart1.z
		val y = -cart1.x*cart2.z+cart2.x*cart1.z
		val z = cart1.x*cart2.y-cart2.x*cart1.y
		Vector3(x,y,z)	
	}


	private[this] def angleNormalise( normLNS : Vector3, normLOC : Vector3) = {
		val scaleLoc = normLOC.scale(-normLNS)
		val num_acos =  scaleLoc.x + scaleLoc.y// normLOC.x*normLNS.x + normLOC.y*normLNS.y + normLOC.z*normLNS.z // normLOC.scale(normLNS)
		val den_acos = normLOC.magnitude * normLNS.magnitude//sqrt(normLOC.x*normLOC.x + normLOC.y*normLOC.y + normLOC.z*normLOC.z) //sqrt(normLOC.magnitude)
		val val_acos = num_acos/den_acos
		val acosfinal = if (val_acos >= 0) atan(sqrt(1-val_acos*val_acos)/val_acos) else Pi + atan(sqrt(1-val_acos*val_acos)/val_acos)
		180*acosfinal/Pi
	}


	def randomizePosition(minDeg : Double, maxDeg : Double) = {
		Random.setSeed(((random*1024561)*100).toLong)
		def randomBetween(minDeg : Double, maxDeg : Double) : Double = { minDeg + (Random.nextDouble)*(maxDeg - minDeg)}
		val longRandom = if(Random.nextInt < 0) -randomBetween(minDeg,maxDeg) else randomBetween(minDeg,maxDeg)
		val latRandom  = if(Random.nextInt < 0) -randomBetween(minDeg,maxDeg) else randomBetween(minDeg,maxDeg)
		this.longitude = this.longitude + longRandom
		this.latitude  = this.latitude  + latRandom
		this.latRadian = latitude.toRadians
		this.coslat    = cos(latitude.toRadians)
		this.sinlat    = sin(latitude.toRadians)
		this.name = "_RANDOMIZED_"+this.name
		memoAngle = HashMap.empty[GpsPoint, Double] //reset du cache, this a changé
	}

	def angle45 ( otherObjet : GpsPoint  ) : (Double, Double) = {
		val (lat1rv,lon1rv,lat2rv,lon2rv) = (toRadians(latitude),toRadians(longitude),toRadians(otherObjet.latitude),toRadians(otherObjet.longitude))
		val cart1 = toCartesianVect(lat1rv, lon1rv)
		val cart2 = toCartesianVect(lat2rv, lon2rv)
		val anglebrut = angleNormalise(getNormLNS(cart2),calcNormLoc(cart1,cart2))
		if (anglebrut % 90 > 45)  (anglebrut, 90 - (anglebrut%90)) else (anglebrut,(anglebrut%90))
	}


    
    def simpleDistance(point: GpsPoint) = {
        val R = 6367.532
        val pi = 3.141592653589793
        //val phi1 = latitude.toRadians
        //val phi2 = point.latitude.toRadians
        val deltaPhi = point.coslat - this.coslat
        val deltaLambda = (point.longitude - longitude).abs.toRadians
        val a = sin(deltaPhi / 2) * sin(deltaPhi / 2) + this.coslat * point.coslat * sin(deltaLambda / 2) * sin(deltaLambda / 2)
        val c = 2 * atan2(sqrt(a), sqrt(1 - a))
        R * c
    }
    

//TODO : memoizer simpleAngle

    /*private*/ var memoAngle : HashMap[GpsPoint, Double] = HashMap.empty[GpsPoint, Double]
    var latRadian : Double = 0.0
    var coslat    : Double = 0.0
    var sinlat    : Double = 0.0
    
	
    def simpleAngle(point: GpsPoint) : Double = {		
        def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }
	//NOTE: C'est plus rentable de faire le calcul, le hash prend un temps fou !
	// Probablement un max de cache miss...
	//if (memoAngle.contains(point)) {
	//	memoAngle(point)
	//} else {
		//println("calcul angle")
		//val phi1 = latitude.toRadians
		//val phi2 = point.latitude.toRadians
		var deltaLambda = (point.longitude-longitude).toRadians
		var y = sin(deltaLambda) * point.coslat;
		var x = this.coslat*point.sinlat - sinlat*point.coslat*cos(deltaLambda);
		var theta = (360 + atan2(y, x).toDegrees) % 360;
		val angle45c = if (theta  % 90 > 45)   90 - (theta % 90) else theta % 90
	//	memoAngle += (point -> roundAt(2)(angle45c)) 
		roundAt(2)(angle45c)
	//}
    }

    def parseCsvLine(s: String) = {
		//println(s.replaceAll(",", "."))
        val regex = "([_\\w-]+)\\s+(\\d+\\.\\d+)\\s+N\\s+([\\d-]+\\.\\d+)\\s*".r
        val list = regex.findAllIn(s).matchData.toList.head.subgroups
	this.latitude = list(1).toDouble
	this.longitude = list.last.toDouble
	this.name =  list.head
	this.latRadian = latitude.toRadians
	this.coslat    = cos(latitude.toRadians)
	this.sinlat    = sin(latitude.toRadians)
    }

   def fromDecimalGPSPoint(s: String) = {
	   println(s)
	   val regex = """(.+?)(?:\s*,\s*)(\d+\.\d+)\s*,\s*([\d-]+\.\d+)\s*""".r
	   try {
		   val list = regex.findAllIn(s).matchData.toList.head.subgroups
		   this.latitude = list(1).toDouble
		   this.longitude = list.last.toDouble
		   this.name =  list.head
		   this.latRadian = latitude.toRadians
		   this.coslat    = cos(latitude.toRadians)
		   this.sinlat    = sin(latitude.toRadians)
	   } catch { case e : Throwable => println("Regexp Failed: '"+ s+"'")}
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

   def isAngleRemarquable(point: GpsPoint, precision : Double) : Boolean = {
	testAngles(this.simpleAngle(point),precision)
   }



    override def toString: String = {
        this.name.concat(" ".concat(this.longitude.toString.concat(" ".concat(this.latitude.toString))))
    }
}


