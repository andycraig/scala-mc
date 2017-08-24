/*
Functional MCMC implementations building on those from darrenjw's blog:
https://darrenjw.wordpress.com/2010/08/15/metropolis-hastings-mcmc-algorithms/
*/

import breeze.linalg._
import breeze.stats.distributions._

object mc {
  def main(args: Array[String]): Unit = {
		println("=== MCMC ===")
		// Define target, which is just a function Double -> Double.
		val target = MCMC.logPriorTimesLik(
			prior = Gaussian(-1.0, 1.0),
			lik = (theta: Double) => Gaussian(theta, 1.0),
			data = DenseVector(0.01, -0.3, 1)) _ // Underscore because it is function.
		val kernel = Gaussian(0.0, 1.0)
		println("Fitting...")
		MCMC.metrop7(kernel, target).take(20).foreach(println)
		println("Done.")
		println("=== Particle Filter ===")

		println("Filtering...")
		val particles = ParticleFilter.particleFilter(
			ys = Seq(1, 1.5, 0.5),
			moveDist = (mu: Double) => Gaussian(mu, 1.0),
			obsDist = (mu: Double) => Gaussian(mu, 0.5),
			x_1 = Seq(0.0, 0.5, 1, 1.5, 2.0))
		println(particles)
		println("Done.")
  }

}
