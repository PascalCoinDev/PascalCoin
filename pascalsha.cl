static inline uint rotr64( __const ulong w, __const unsigned c ) { return ( w >> c ) | ( w << ( 64 - c ) ); }
static inline uint ROTRIGHT( __const uint w, __const unsigned c) { return (w >> c) | (w << (32 - c) ); }
static inline uint ror( __const uint w, __const unsigned c) { return (w >> c) | (w << (32 - c) ); }
static inline uint SIG0( __const uint x) { return ROTRIGHT(x, 7) ^ ROTRIGHT(x, 18) ^ ((x) >> 3); }
static inline uint SIG1( __const uint x) { return ROTRIGHT(x, 17) ^ ROTRIGHT(x, 19) ^ ((x) >> 10); }
static inline uint SIG0c( __const uint x) { return ROTRIGHT(x, 7) ^ ROTRIGHT(x, 18) ^ ((x) >> 3); }
static inline uint SIG1c( __const uint x) { return ROTRIGHT(x, 17) ^ ROTRIGHT(x, 19) ^ ((x) >> 10); }

__constant static const uint k[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  };


__kernel void pascalcoin(__global uint *midstateIn, __global int *nonceOut) {
    /*
    midstateIn is an array[0..28]
    midstateIn[0..15] = data for last chunk
    midstateIn[16..23] = previous chunk result
    midstateIn[24] = Position to save nOnce
    midstateIn[25] = Highest 10 bits for nOnce
    midstateIn[26..28] = Mask (obtained  from target_pow)

    When found a valid nOnce, is returned at nonceOut
    */
  uint buffer[16];
  uint midstate[8];

  // midstateIn[25] high-order 10 bits for nOnce, we must bit shift left value 22 bits.
  // get_global_id(0) is the current ROUND. ROUND is a value between 0..(2^22)-1
  uint nonce = (midstateIn[25] << 22) + (uint)get_global_id(0);

  uint whereToSavenOnce = (midstateIn[24]);

  uint maskTargetH0 = (midstateIn[26]);
  uint maskTargetH1 = (midstateIn[27]);
  uint maskTargetH2 = (midstateIn[28]);

  midstate[0] = midstateIn[16];
  midstate[1] = midstateIn[17];
  midstate[2] = midstateIn[18];
  midstate[3] = midstateIn[19];
  midstate[4] = midstateIn[20];
  midstate[5] = midstateIn[21];
  midstate[6] = midstateIn[22];
  midstate[7] = midstateIn[23];

  int j = 0;

  for (j = 0; j < 16; j++)
  {
	  buffer[j] = midstateIn[j];
  }

  uint block[64];

  uint temp1;
  uint temp2;
  uint S0;
  uint S1;

  uint h0, h1, h2, h3, h4, h5, h6, h7;

  uint a, b, c, d, e, f, g, h;

  h0 = midstate[0];
  h1 = midstate[1];
  h2 = midstate[2];
  h3 = midstate[3];
  h4 = midstate[4];
  h5 = midstate[5];
  h6 = midstate[6];
  h7 = midstate[7];

  a = h0;
  b = h1;
  c = h2;
  d = h3;
  e = h4;
  f = h5;
  g = h6;
  h = h7;

  buffer[whereToSavenOnce] = nonce;

  for (j = 0; j < 16; j++)
  {
	  block[j] = buffer[j];
  }

  for (j = 16; j < 64; j++)
  {
	  block[j] = block[j - 16] + block[j - 7] + SIG1c(block[j - 2]) + SIG0c(block[j - 15]);
  }

  for (j = 0; j < 64; j++)
  {
	  S1 = (ror(e, 6)) ^ (ror(e, 11)) ^ (ror(e, 25));
	  temp1 = h + S1 + ((e & f) ^ ((~e) & g)) + k[j] + block[j];
	  S0 = (ror(a, 2)) ^ (ror(a, 13)) ^ (ror(a, 22));
	  temp2 = S0 + (((a & b) ^ (a & c) ^ (b & c)));

	  h = g;
	  g = f;
	  f = e;
	  e = d + temp1;
	  d = c;
	  c = b;
	  b = a;
	  a = temp1 + temp2;
  }

  h0 += a;
  h1 += b;
  h2 += c;
  h3 += d;
  h4 += e;
  h5 += f;
  h6 += g;
  h7 += h;

  block[0] = h0;
  block[1] = h1;
  block[2] = h2;
  block[3] = h3;
  block[4] = h4;
  block[5] = h5;
  block[6] = h6;
  block[7] = h7;
  block[8] = 0x80000000;
  block[9] = 0x00000000;
  block[10] = 0x00000000;
  block[11] = 0x00000000;
  block[12] = 0x00000000;
  block[13] = 0x00000000;
  block[14] = 0x00000000;
  block[15] = 0x00000100;

  h0 = a = 0x6a09e667;
  h1 = b = 0xbb67ae85;
  h2 = c = 0x3c6ef372;
  h3 = d = 0xa54ff53a;
  h4 = e = 0x510e527f;
  h5 = f = 0x9b05688c;
  h6 = g = 0x1f83d9ab;
  h7 = h = 0x5be0cd19;

  for (j = 16; j < 64; j++)
  {
	  block[j] = block[j - 16] + block[j - 7] + SIG1c(block[j - 2]) + SIG0c(block[j - 15]);
  }

  for (j = 0; j < 64; j++)
  {
	  S1 = (ror(e, 6)) ^ (ror(e, 11)) ^ (ror(e, 25));
	  temp1 = h + S1 + ((e & f) ^ ((~e) & g)) + k[j] + block[j];
	  S0 = (ror(a, 2)) ^ (ror(a, 13)) ^ (ror(a, 22));
	  temp2 = S0 + (((a & b) ^ (a & c) ^ (b & c)));

	  h = g;
	  g = f;
	  f = e;
	  e = d + temp1;
	  d = c;
	  c = b;
	  b = a;
	  a = temp1 + temp2;
  }

  h0 += a;
  h1 += b;
  h2 += c;
  h3 += d;
  h4 += e;
  h5 += f;
  h6 += g;
  h7 += h;

  uint target0 = h0 & maskTargetH0;
  uint target1 = h1 & maskTargetH1;
  uint target2 = h2 & maskTargetH2;
  if (target0 == 0 && target1 == 0 && target2 == 0)
  {
	  *nonceOut = nonce;
  }
}
