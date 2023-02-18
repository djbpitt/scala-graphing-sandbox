/*
* 2023-02-18
* Corrects broken example at
* https://docs.scala-lang.org/scala3/book/types-adts-gadts.html
*
* To run:
* scalac planets.scala
* scala planets.scala 100
* */
enum Planet(mass: Double, radius: Double):

  private final val G = 6.67300E-11
  def surfaceGravity = G * mass / (radius * radius)
  def surfaceWeight(otherMass: Double) =  otherMass * surfaceGravity

  case Mercury extends Planet(3.303e+23, 2.4397e6)
  case Venus   extends Planet(4.869e+24, 6.0518e6)
  case Earth   extends Planet(5.976e+24, 6.37814e6)

@main def main(earthWeight: Double): Unit =
  val mass = earthWeight / Planet.Earth.surfaceGravity
  for (p <- Planet.values)
    println(s"Your weight on $p is ${p.surfaceWeight(mass)}")

