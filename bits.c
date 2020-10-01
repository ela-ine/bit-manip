/* 
 * CS:APP Data Lab 
 * 
 * Elaine Wan (CNET: elainewan)
 * 
 * bits.c - Source file with your solutions to the project.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the project by
editing the collection of functions in this source file.

CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. The maximum number of ops for each function is given in the
     header comment for each function.

/*
 * STEP 2: Modify the following functions according to the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the btest checker to verify that your solutions produce
 *      the correct answers.
 */


#endif

/************************************************ 
 * absVal - absolute value of x
 *   Example: absVal(-1) = 1.
 *   You may assume -TMax <= x <= TMax
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4
 */
int absVal(int x) {
  // if sign==0 return x, else flip x to positive
  int sign = x>>31;
  int invert = ~x + 1;
  
  return (x&(~sign))|(invert&sign);
}

/************************************************ 
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000,0x80000000) = 0,
 *            addOK(0x80000000,0x70000000) = 1, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y) {
  /* signs of x and y must be different or sum should keep same sign as either x or y, i.e. if x and y are both positive, the sum shouldn't be negative */
  int xmsb = (x>>31);
  int ymsb = (y>>31);
  int sum_msb = (x+y)>>31;
  
  return !!(xmsb^ymsb)|!((sum_msb^xmsb)&(sum_msb^ymsb));
  // if msbs are not the same, else (if msbs same) sum keeps sign of x or y
}

/************************************************ 
 * allEvenBits - return 1 if all even-numbered bits in word set to 1
 *   Examples allEvenBits(0xFFFFFFFE) = 0, allEvenBits(0x55555555) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allEvenBits(int x) {
  // create constant with | and << then mask
  int c = 0x55|(0x55<<8)|(0x55<<16)|(0x55<<24); 
  int even = c & x;
  even = !(even^c); // if even == c
  
  return even;
}

/************************************************
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int bang(int x) {
  // check if x = 0, i.e. if ~x = ~(~x+1) is true

  int not = ~x; // will equal to -1 (0xFFFFFFFF) if 0
  int neg = not+1; // will equal to 0 (0x00000000) if 0
  // so: if x = 0, not = ~neg, ~not = neg => x = neg
  // else: if x != 0, not != ~neg, neg sometimes = x (ex: 0x80000000)

  return ((not&~neg)>>31)&1;
}

/************************************************
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {
  // basically brute forced it using the hint from FAQ
  int mask = (0x11)|(0x11<<8)|(0x11<<16)|(0x11<<24);
  int x1 = x&mask;
  int x2 = (x>>1)&mask;
  int x3 = (x>>2)&mask;
  int x4 = (x>>3)&mask;
  int sum = x1+x2+x3+x4;
  int res = sum&0xf;
  res +=((sum>>4)&0xf);
  res +=((sum>>8)&0xf);
  res += ((sum>>12)&0xf);
  res += ((sum >>16)&0xf);
  res += ((sum>>20)&0xf);
  res +=((sum>>24)&0xf);
  res +=((sum>>28)&0xf);

  return res; 
}

/************************************************
 * bitNor - ~(x|y) using only ~ and & 
 *   Example: bitNor(0x6, 0x5) = 0xFFFFFFF8
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitNor(int x, int y) {
  /* apply nand to inputs such that only inputs of 0 return 1.
     then using and to return 1 only when both inputs are 0  */
  int nandx = ~(x&x);
  int nandy = ~(y&y);
  int retval = nandx&nandy;
  
  return retval;
}

/************************************************
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {
  // subtract bytes then add them back in
  int bitn = n<<3;
  int bitm = m<<3;
  int atm = (x>>bitm)&0xFF; // masks for m
  int atn = (x>>bitn)&0xFF; // masks for n
  int maskn = (0xFF<<bitn);
  int maskm = (0xFF<<bitm);
  int newx = x+(~((maskn&x)+(maskm&x))+1); // subtracts n and m from x
  newx += (atm<<bitn) + (atn<<bitm);

  //printf("%x, %x\n", ~(x & mask)+1, newx);

  return newx;
}

/************************************************ 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  // create mask (either all 1s or 0s) based on x for y, invert mask for z
  int tf = (!!x) << 31 >> 31;
  return (tf&y)|(~tf&z);
}

/************************************************
 * ezThreeFourths - multiplies by 3/4 rounding toward 0,
 *   Should exactly duplicate effect of C expression (x*3/4),
 *   including overflow behavior.
 *   Examples: ezThreeFourths(11) = 8
 *             ezThreeFourths(-9) = -6
 *             ezThreeFourths(1073741824) = -268435456 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
int ezThreeFourths(int x) {
  // using left and right shift, multiply by 3 then divide by 4.
  // if x*3<0, +3 to shift bits up before dividing
  int mult3 = (x<<1)+x;
  int sign = (mult3 >> 31);
  int div4 = ((mult3&~sign)|((mult3+3)&sign))>>2; 

  return div4;
}

/************************************************ 
 * fitsBits - return 1 if x can be represented as an 
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n) {
  /* checks whether or not truncated x will keep same sign bit - if yes, two's 
     complement will be no problem, otherwise, sign bit will be cut off */

  int cutoff = 33 + ~n; // 32 - n
  return !(((x << cutoff)>>cutoff)^x);
}

/************************************************
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n) {
  // right-shift x up until desired bit (n*8 == n<<3) and mask
  int retval = (x >> (n<<3))&0xFF;

  return retval;
}

/************************************************ 
 * greatestBitPos - return a mask that marks the position of the
 *               most significant 1 bit. If x == 0, return 0
 *   Example: greatestBitPos(96) = 0x40
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 70
 *   Rating: 4 
 */
int greatestBitPos(int x) {
/* shift x over until all bits to the right of msb are 1, then get rid of
   those bits */
  int sign = x & (1<<31);
  int res = x | x >> 1; // 8000 -> c000
  res = res | res >> 2; // c000 -> f000
  res = res | res >> 4; // ff00
  res = res | res >> 8; // ffff
  res = res | res >> 16; // ffff ffff

  return (sign)|(res+(~(res>>1)+1));
}

/************************************************
 * implication - return x -> y in propositional logic - 0 for false, 1
 * for true
 *   Example: implication(1,1) = 1
 *            implication(1,0) = 0
 *   Legal ops: ! ~ ^ |
 *   Max ops: 5
 *   Rating: 2
 */
int implication(int x, int y) {
  /* exploiting xnor to check for the TT/FF cases, and in the other cases,
     output is determined by y */
  int xor = (x^y);
  int res = y|!(xor);
  
  return res;
}

/************************************************ 
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x) {
  /* checking 1st and 2nd place of hex: 2nd must == 3, 1st <= 9 */
  int y = x&0xF0;
  int i = x&0xF;
  int rest = x>>8;
  int ans = (!(y^0x30))&(!rest)&(!((10+(~i))&(1<<31)));

  return ans;
}

/************************************************ 
 * isEqual - return 1 if x == y, and 0 otherwise 
 *   Examples: isEqual(5,5) = 1, isEqual(4,5) = 0
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int isEqual(int x, int y) {
  // exploiting xor logic
  int xor = x^y;
  int res = !(xor);
  return res;
}

/************************************************
 * isLess - if x < y  then return 1, else return 0 
 *   Example: isLess(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLess(int x, int y) {
  // if x and y have different signs, return result accordingly, otherwise x-y<0
  int diff = x + (~y)+1; // x-y
  int sign = (diff>>31)&1; 
  int xsign = (x>>31)&1;
  int ysign = (y>>31)&1;
  int same = !(xsign^ysign); // if xsign = ysign
  int implic = xsign|same;

  return (same&sign)|((!same)&implic);
}

/************************************************
 * isNonNegative - return 1 if x >= 0, return 0 otherwise 
 *   Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 3
 */
int isNonNegative(int x) {
  // checks sign bit of x
  return !(x&(1<<31));
}

/************************************************
 * isPower2 - returns 1 if x is a power of 2, and 0 otherwise
 *   Examples: isPower2(5) = 0, isPower2(8) = 1, isPower2(0) = 0
 *   Note that no negative number is a power of 2.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int isPower2(int x) {
  // check that there is only one 1 in x: if x==0, x-1 = 01...1 or 0
  int minusone = x + ~0; 
  int res = minusone & x;
  int sign = x>>31;

  return (!res)&!sign&!!(x^0);
}

/************************************************
 * isTmin - returns 1 if x is the minimum, two's complement number,
 *     and 0 otherwise 
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmin(int x) {
  /* the min two's complement number, when multiplied by 2, should overflow 
     e.g. if 0b1000 is max 2's complement, 0b1000+0b1000= 0b10000 
     so if x == min, !(x+x)==1 and !x==0 */
  int timestwo = x+x;

  return (!x^(!timestwo));
}

/************************************************
 * minusOne - return a value of -1 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 2
 *   Rating: 1
 */
int minusOne(void) {
  // -1 = ~0b0001 + 0b0001 = 0b1111 = ~0
  return (~0);
}

/************************************************
 * rotateLeft - Rotate x to the left by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateLeft(0x87654321,4) = 0x76543218
 *   Legal ops: ~ & ^ | + << >> !
 *   Max ops: 25
 *   Rating: 3 
 */
int rotateLeft(int x, int n) {
  // shift x left by n then add back in the "overflow" in lower places
  int new = x << n;
  int shift = 33 + ~n;
  int over = (x >> shift)&(~((~0)<<n));

  return new+over;
}

/************************************************
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x60000000) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
  // use shift for multiplication, if sign changes --> overflow
  int mul2 = x<<1;
  int xmsb = x>>31;
  int mul2msb = mul2>>31;
  int ifsame = ~(xmsb^mul2msb);
  int min = 1<<31;
  int overflow = (xmsb&min)|(~xmsb&(min+1+~1));
  int res = (mul2&ifsame)|(overflow&~ifsame);

  return res;
}
