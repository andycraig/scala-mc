/*
Functional MCMC implementations building on those from darrenjw's blog:
https://darrenjw.wordpress.com/2010/08/15/metropolis-hastings-mcmc-algorithms/
*/

import breeze.linalg._
import breeze.stats.distributions._

object SMC {
	def smc(qs: DenseVector[ContinuousDistr[Double]],
					x1: Double,
				 	data: DenseVector[Double]): ? = {
		//TODO Stream.iterate smcStep
	}

	// Take in a distribution q and particles.
	// @return N new equally-weighted particles.
	def smcStep(q: ContinuousDistr[Double],
							Xbar_1_to_n_minus_1: DenseVector[Double]): DenseVector[Double] = {
		Xbar_1_to_n_minus_1.foreach //TODO Sample a new particular from q.
		//TODO Compute weights
		smcResample //TODO
		//TODO Return result of smcResample.
	}

	def smcGetWeights(): Something = {
		//TODO Implement
	}

	// Given particles and associated weights,
	// @return Resampled particles with equal weights.
	def smcResample(weights_xs: DenseVector[(Double, Double)]): DenseVector[(Double, Double)] = {
		//TODO Implement
	}
}
object MCMC {

	def logPriorTimesLik(prior: ContinuousDistr[Double],
										lik: (Double) => ContinuousDistr[Double],
										data: DenseVector[Double])(theta: Double): Double = {
		val priorComponent = prior.logPdf(theta)
		val likComponent = data.map(x => lik(theta).logPdf(x)).reduce(_+_)
		priorComponent + likComponent
	}

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
}

object mc {
  def main(args: Array[String]): Unit = {
		// Define target, which is just a function Double -> Double.
		val target = logPriorTimesLik(
			prior = Gaussian(-1.0, 1.0),
			lik = (theta: Double) => Gaussian(theta, 1.0),
			data = DenseVector(0.01, -0.3, 1)) _ // Underscore because it is function.
		val kernel = Gaussian(0.0, 1.0)
		println("Fitting...")
		MCMC.metrop7(kernel, target).take(20).foreach(println)
		println("Done.")
  }

}
