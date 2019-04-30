# Overview

This project contains a streaming, pipelined Fast Fourier Transform (FFT).
Twiddle factors are hard-coded for you.
The transform is split into pipelined Biplex FFTs and a direct form FFT to multiplex the logic for large FFT sizes.

# Specifications

## Parameters
- `n`: This is the size (points) of the FFT.
- `pipelineDepth`: This is the number of pipeline registers to add, up to the number of butterfly stages of the Cooley-Tukey algorithm.
- `lanes`: This is the number of parallel input lanes for time-series data. Must be a power of 2, a minimum of 2, and maxiumum of `n`.

## IOs
- `in`: Time-domain ValidWithSync input of type `Vec(lanes, genIn)`
- `out`: Frequency-domain ValidWithSync output of type `Vec(lanes, genOut)`


## Signaling

### Bits

It is expected that the bits inputs contain time-series data time-multiplexed on the inputs, such that on the first cycle are values x[0], x[1], …, x[p-1], then the next cycle contains x[p], x[p+1], … and this continues until the input is x[n-p], x[n-p+1], …, x[n-1]. 
The outputs are scrambled spectral bins. 
Since there is some unscrambling between the biplex and direct form FFT, the output indices are not purely bit reversed. 
The unscrambling routine is hard to describe in words, but the FFT tester has a definition to handle this, copied here.
The FFT size is inferred from the length of `in`, and `p` gives the number of lanes in the design.

```
def unscramble(in: Seq[Complex], p: Int): Seq[Complex] = {
  val n = in.size
  val bp = n/p
  val res = Array.fill(n)(Complex(0.0,0.0))
  in.grouped(p).zipWithIndex.foreach { case (set, sindex) =>
    set.zipWithIndex.foreach { case (bin, bindex) =>
      if (bp > 1) {
        val p1 = if (sindex/(bp/2) >= 1) 1 else 0
        val new_index = bit_reverse((sindex % (bp/2)) * 2 + p1, log2Up(bp)) + bit_reverse(bindex, log2Up(n))
        res(new_index) = bin
      } else {
        val new_index = bit_reverse(bindex, log2Up(n))
        res(new_index) = bin
      }
    }
  }
  res
}
```

### Valid

The FFT delays the input valid by a value equal to the total data delay (biplex FFT delay + pipeline depth).
When valid is low, the FFT creates zeros at its input.
Internal counters continue to count, flushing out extant data.
The shift register delaying the valid signal is set to all 0s during reset.

### Sync

The shift register delaying the sync signal is set to all 0s during reset.
The first input sync signal will flush through, synchronizing all the FFT butterflies with the first dataset. 
The input sync is expected to be periodic in the size of the FFT (n) divided by the number of input lanes (p). 
Sync should be high on the last cycle of the spectrum. 
The new spectrum starts on the next valid cycle. 
When n=p, sync should always be high when valid is high.

## Implementation

The FFT supports any power of two size of 4 or greater (n >= 4). 
The input rate may be divided down, resulting in a number of parallel input lanes different from the FFT size. 
But the input lanes (p) must be a power of 2, greater than or equal to 2, but less than or equal to the FFT size. 

When the number of parallel inputs equals the FFT size, a simple, direct form, streaming FFT is used, as shown below. 
The dotted lines mark "stage" boundaries, or places where pipeline registers may be inserted. 
The input will never be pipelined, but the output might be pipelined based on your desired pipeline depth. 
Pipeline registers are automatically inserted in reasonable locations.

![In-place FFT](/doc/inplacefft.png?raw=true)

When the input is serialized, the FFT may have fewer input lanes than the size of the FFT. 
In this case, the inputs are assumed to arrive in time order, time-multiplexed on the same wires. 
To accommodate this time multiplexing, the FFT architecture changes. 
Pipelined biplex FFTs are inserted before the direct form FFT. 
These FFTs efficiently reuse hardware and memories to calculate the FFT at a slower rate but higher latency. 
The figure below shows their architecture, as taken from the JPL technical report. 
Since the channels are adjacent, N = n/2, where n is the size of the biplex FFT. 
The solid boxes are shift registers of delay shown, and the boxes with dotted lines in them are 2-input barrel shifters, periodically crossing or passing the inputs at a rate shown. 
The Xs in the diagram are butterflies. 
An extra shift register on input 1 of size N/2 aligns the adjacent channels, and extra shift registers of n/2 at the output unscramble the data before they arrive at the direct form FFT. 
Pipeline registers may be inserted after each butterfly, but never at the input or output.

![Biplex FFT](/doc/biplexfft.png?raw=true)

A final direct form FFT sits at the output of the biplex FFTs, finishing the Fourier transform. 
Thus the overall architecture looks like below. 
Pipeline registers favor the direct form FFT slightly, though the critical path through this circuit is still through log2(n) butterflies, so one pipeline register per stage (a pipeline depth of log2(n)) is recommended.

![Split FFT](/doc/splitfft.png?raw=true)

## Testing
- Unit test: `sbt "testOnly mimo.FFTSpec"`. Test exercises a 256-point fixed-point FFT with IO width of 16 and binary point of 12. 256 real and imaginary values are loaded in via the files `src/test/resources/FFT_Real.csv` and `src/test/resources/FFT_Imag.csv`. These values are then scaled by 1/256 as in the tail, to avoid overflowing intermediate values in the FFT. The resulting spectrum is compared to a the output of Scala Breeze's fourierTr function.