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
					obsDist: (Double) => ContinuousDistr[Double]): Seq[Seq[Double]] = {
		val stepFn = step(moveDist = moveDist, obsDist = obsDist) _
		val particles = ys.scanLeft(x_1){ (x_n_minus_1, y) => stepFn(x_n_minus_1)(y) }
		particles
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
		val samples = X_n_minus_1.map(moveDist(_).draw)
		val unnormalised_weights = samples.map(obsDist(y).pdf(_))
		val weights_sum = breeze.linalg.sum(unnormalised_weights)
		val weights = unnormalised_weights.map(_ / weights_sum)
		val sampleDist = Multinomial[DenseVector[Double], Int](DenseVector[Double](weights:_*))
		val X_n = for (unused <- samples) yield samples(sampleDist.draw)
		X_n
	}
}
