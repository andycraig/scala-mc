/*
Functional MCMC implementations building on those from darrenjw's blog:
https://darrenjw.wordpress.com/2010/08/15/metropolis-hastings-mcmc-algorithms/
*/

import breeze.linalg._
import breeze.stats.distributions._

object ParticleFilter {

	// @param obsDist The observation distribution, parameterised by the mean.
	// @param moveDist The state evolution distribution, parameterised by the mean.
	def particleFilter(
					x1: Double,
					ys: Seq[Double],
					moveDist: (Double) => ContinuousDistr[Double],
					obsDist: (Double) => ContinuousDistr[Double]): ? = {
		ys.SOMETHING(step(moveDist = moveDist, obsDist = obsDist))
		//Stream.iterate((x, oldll))(step(moveDist = moveDist, obsDist = obsDist)) map (_._1)
	}

	/* Takes in a pair of distributions, and returns a function that takes in
		particles, and returns a function that takes in a datum and returns new particles.
		Essentially:
		1. It is initialised with the distributions.
		2. That is initialised with particles.
		3. That is passed a datum, returning new particles.
		So it can 'crawl along' a series of data points, updating
	*/ @return N new equally-weighted particles.
	def step(moveDist: (Double) => ContinuousDistr[Double],
						obsDist: (Double) => ContinuousDistr[Double])
						(X_n_minus_1: Seq[Double]),
						(y: Double): Seq[Double] = {
		val samples = X_n_minus_1.map(moveDist(X_n_minus_1).pdf(_))
		val unnormalised_weights = samples.map(obsDist(y).pdf(_)) //TODO Sample a new particular from q.
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
