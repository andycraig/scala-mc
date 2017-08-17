/*
Functional MCMC implementations building on those from darrenjw's blog:
https://darrenjw.wordpress.com/2010/08/15/metropolis-hastings-mcmc-algorithms/
*/

import breeze.linalg._
import breeze.stats.distributions._

object MCMC {

	//TODO Currently target is just a function Double -> Double.
	// Can this be made more strict, so that it is a log probability?

	// Altered version of newState.
	// I think nextState(eps) returns a function ((Double, Double) -> (Double, Double)).
	def nextState(kernel: ContinuousDistr[Double],
						targetLogPdf: (Double) => Double)(state: (Double, Double)): (Double, Double) = {
		val x = state._1
		val oldll = state._2
		val can = x + kernel.draw
		val loglik = targetLogPdf(can)
		val loga = loglik - oldll
		if (math.log(Uniform(0.0, 1.0).draw) < loga) (can, loglik) else (x, oldll)
	}

	def metrop7(kernel: ContinuousDistr[Double],
						targetLogPdf: (Double) => Double,
						x: Double = 0.0,
						oldll: Double = Double.MinValue): Stream[Double] =
		Stream.iterate((x, oldll))(nextState(kernel, targetLogPdf)) map (_._1)


  def main(args: Array[String]): Unit = {
		// Some made-up data.
		val data = DenseVector(0.01, -0.3, 1)
		// Prior with wrong mean.
		val prior = Gaussian(-1.0, 1.0)
		// like is a function of theta, the parameter we will fit.
		// (Assume we know the variance.)
		val lik = (theta: Double) => Gaussian(theta, 1.0)
		// Define target, which is just a function Double -> Double.
		//TODO Make a function that takes a prior, a likelihood and data, and returns this function.
		val target = (theta: Double) => (prior.logPdf(theta) +
							data.map(lik(theta).logPdf(_)).reduce(_+_))
		val kernel = Gaussian(0.0, 1.0)
		println("Fitting...")
		metrop7(kernel, target).take(10)
		print("Done.")

  }

}
