package org.messagedelanuitdestemps.anglestest


import math._
import scala.io.Source



class GpsPoint(csvLine: String) {
    val position: (Double, Double, String) = parseCsvLine(csvLine)
    val latitude: Double = position._1
    val longitude: Double = position._2
    val name: String = position._3
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
        val phi1 = latitude.toRadians
        val phi2 = point.latitude.toRadians
        val deltaPhi = phi2 - phi1
        val deltaLambda = (point.longitude - longitude).abs.toRadians
        val a = sin(deltaPhi / 2) * sin(deltaPhi / 2) + cos(phi1) * cos(phi2) * sin(deltaLambda / 2) * sin(deltaLambda / 2)
        val c = 2 * atan2(sqrt(a), sqrt(1 - a))
        R * c
    }
    
    def simpleAngle(point: GpsPoint) : Double = {		
        def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }
		val phi1 = latitude.toRadians
		val phi2 = point.latitude.toRadians
		var deltaLambda = (point.longitude-longitude).toRadians
		var y = sin(deltaLambda) * cos(phi2);
		var x = cos(phi1)*sin(phi2) - sin(phi1)*cos(phi2)*cos(deltaLambda);
		var theta = (360 + atan2(y, x).toDegrees) % 360;
		val angle45c = if (theta  % 90 > 45)   90 - (theta % 90) else theta % 90
		roundAt(2)(angle45c)
	}

    def parseCsvLine(s: String): (Double, Double, String) = {
        val regex = "(\\w+)\\s+(\\d+\\.\\d+)\\s+N\\s+(\\d+\\.\\d+)".r
        val list = regex.findAllIn(s.replaceAll(",", ".")).matchData.toList.head.subgroups
        (list(1).toDouble, list.last.toDouble, list.head)
    }

    override def toString: String = {
        this.name.concat(" ".concat(this.longitude.toString.concat(" ".concat(this.latitude.toString))))
    }
}


