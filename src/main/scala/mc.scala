/*
Functional MCMC implementations building on those from darrenjw's blog:
https://darrenjw.wordpress.com/2010/08/15/metropolis-hastings-mcmc-algorithms/
*/

import breeze.linalg._
import breeze.stats.distributions._

object ParticleFilter {

	// @param x_1 The initial particles.
	// @param ys The data for fitting.
	// @param obsDist The observation distribution, with a single parameter.
	// @param moveDist The state evolution distribution, with a single parameter.
	// @return The particles at the end.
	def particleFilter(
					x_1: Seq[Double],
					ys: Seq[Double],
					moveDist: (Double) => ContinuousDistr[Double],
					obsDist: (Double) => ContinuousDistr[Double]): Seq[Double] = {
		val stepFn = step(moveDist = moveDist, obsDist = obsDist) _
		ys.foldLeft(x_1){ (x_n_minus_1, y) => stepFn(x_n_minus_1)(y) }
	}

	/* Takes in a pair of distributions, and returns a function that takes in
		particles, and returns a function that takes in a datum and returns new particles.
		Essentially:
		1. It is initialised with the distributions.
		2. That is initialised with particles.
		3. That is passed a datum, returning new particles.
		So it can 'crawl along' a series of data points, updating
		@return N new equally-weighted particles.
	*/
	def step(moveDist: (Double) => ContinuousDistr[Double],
						obsDist: (Double) => ContinuousDistr[Double])
						(X_n_minus_1: Seq[Double])
						(y: Double): Seq[Double] = {
		//TODO Fix this line. Should this be:
		// val samples = X_n_minus_1.map(moveDist(_).draw)
		// ? Check what it is supposed to be doing.
		val samples = X_n_minus_1.map(moveDist(X_n_minus_1).pdf(_))
		val unnormalised_weights = samples.map(obsDist(y).pdf(_))
		val weights_sum = unnormalised_weights.reduce(_+_)
		val weights = unnormalised_weights.map(_ / weights_sum)
		val sampleDist = Multinomial(weights, samples)
		val X_n = for (unused <- samples_weights) yield sampleDist.draw
		X_n
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
		println("=== MCMC ===")
		// Define target, which is just a function Double -> Double.
		val target = logPriorTimesLik(
			prior = Gaussian(-1.0, 1.0),
			lik = (theta: Double) => Gaussian(theta, 1.0),
			data = DenseVector(0.01, -0.3, 1)) _ // Underscore because it is function.
		val kernel = Gaussian(0.0, 1.0)
		println("Fitting...")
		MCMC.metrop7(kernel, target).take(20).foreach(println)
		println("Done.")
		println("=== Particle Filter ===")

		println("Filtering...")
		val particles = ParticleFilter.ParticleFilter(
			ys = DenseVector(1, 1.5, 0.5),
			moveDist = (mu: Double) => Gaussian(mu, 1.0),
			obsDist = (mu: Double) => Gaussian(mu, 0.5),
			x_1 = Seq(0.0, 0.5, 1, 1.5, 2.0))
		println("Done.")
  }

}
