package main

import java.awt.{Color, Graphics2D}

class Circle(
              var rad: Double,
              var center: V2,
              var v: V2 = V2(0, 0),
              var acc: V2 = V2(0, 0),
//              var mass: Double = 1,
              var restitution: Double = 1,
              var color: Color = Color.BLACK,
              var dynamic: Boolean = true
            ) {

  def intersects(c:V2, r:Double, c2:V2, r2:Double) :Boolean = (c2 - c).length <= r + r2


  def updateMe(delta: Double) = {
    if(dynamic) {
      v = v + (acc + g) * delta
      val newCenter = center + v * delta // + a * t^2 / 2
      val collisions = Graphics.balls.filter(_ != this).filter(c => intersects(newCenter, rad, c.center, c.rad))
      collisions.headOption match {
        case Some(other) =>
          val bv = other.v
          val av = v
          val hab = other.center - newCenter
          val hba = -hab
          val dva_a = hab.normalize * av.proj(hab) * this.restitution
          val dva_b = hab.normalize * av.proj(hab) * (1 - this.restitution)
          val dvb_b = hba.normalize * bv.proj(hba) * other.restitution
          val dvb_a = hba.normalize * bv.proj(hba) * (1 -other.restitution)
          this.v = this.v - 2 * dva_a + dvb_a
          other.v = other.v - 2 * dvb_b + dva_b
          acc = V2(0, 0)
          other.acc = V2(0, 0)

        case None =>
          center = newCenter
      }
    }
  }

  def drawMe(g: Graphics2D): Unit = {
    g.fillOval((center.x - rad) toInt, (center.y - rad) toInt, 2 * rad toInt, 2 * rad toInt)
  }
}



