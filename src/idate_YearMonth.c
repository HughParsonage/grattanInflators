#include "grattanInflator.h"
  // integer values of first of every month
const static int IDATE_BY_YEARMONTH_SINCE_1948_JAN_1ST[1536] = {
    -8036, -8005, -7976, -7945, -7915, -7884, -7854, -7823, -7792, -7762, -7731, -7701,
    -7670, -7639, -7611, -7580, -7550, -7519, -7489, -7458, -7427, -7397, -7366, -7336,
    -7305, -7274, -7246, -7215, -7185, -7154, -7124, -7093, -7062, -7032, -7001, -6971,
    -6940, -6909, -6881, -6850, -6820, -6789, -6759, -6728, -6697, -6667, -6636, -6606,
    -6575, -6544, -6515, -6484, -6454, -6423, -6393, -6362, -6331, -6301, -6270, -6240,
    -6209, -6178, -6150, -6119, -6089, -6058, -6028, -5997, -5966, -5936, -5905, -5875,
    -5844, -5813, -5785, -5754, -5724, -5693, -5663, -5632, -5601, -5571, -5540, -5510,
    -5479, -5448, -5420, -5389, -5359, -5328, -5298, -5267, -5236, -5206, -5175, -5145,
    -5114, -5083, -5054, -5023, -4993, -4962, -4932, -4901, -4870, -4840, -4809, -4779,
    -4748, -4717, -4689, -4658, -4628, -4597, -4567, -4536, -4505, -4475, -4444, -4414,
    -4383, -4352, -4324, -4293, -4263, -4232, -4202, -4171, -4140, -4110, -4079, -4049,
    -4018, -3987, -3959, -3928, -3898, -3867, -3837, -3806, -3775, -3745, -3714, -3684,
    -3653, -3622, -3593, -3562, -3532, -3501, -3471, -3440, -3409, -3379, -3348, -3318,
    -3287, -3256, -3228, -3197, -3167, -3136, -3106, -3075, -3044, -3014, -2983, -2953,
    -2922, -2891, -2863, -2832, -2802, -2771, -2741, -2710, -2679, -2649, -2618, -2588,
    -2557, -2526, -2498, -2467, -2437, -2406, -2376, -2345, -2314, -2284, -2253, -2223,
    -2192, -2161, -2132, -2101, -2071, -2040, -2010, -1979, -1948, -1918, -1887, -1857,
    -1826, -1795, -1767, -1736, -1706, -1675, -1645, -1614, -1583, -1553, -1522, -1492,
    -1461, -1430, -1402, -1371, -1341, -1310, -1280, -1249, -1218, -1188, -1157, -1127,
    -1096, -1065, -1037, -1006, -976, -945, -915, -884, -853, -823, -792, -762,
    -731, -700, -671, -640, -610, -579, -549, -518, -487, -457, -426, -396,
    -365, -334, -306, -275, -245, -214, -184, -153, -122, -92, -61, -31,
    0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334,
    365, 396, 424, 455, 485, 516, 546, 577, 608, 638, 669, 699,
    730, 761, 790, 821, 851, 882, 912, 943, 974, 1004, 1035, 1065,
    1096, 1127, 1155, 1186, 1216, 1247, 1277, 1308, 1339, 1369, 1400, 1430,
    1461, 1492, 1520, 1551, 1581, 1612, 1642, 1673, 1704, 1734, 1765, 1795,
    1826, 1857, 1885, 1916, 1946, 1977, 2007, 2038, 2069, 2099, 2130, 2160,
    2191, 2222, 2251, 2282, 2312, 2343, 2373, 2404, 2435, 2465, 2496, 2526,
    2557, 2588, 2616, 2647, 2677, 2708, 2738, 2769, 2800, 2830, 2861, 2891,
    2922, 2953, 2981, 3012, 3042, 3073, 3103, 3134, 3165, 3195, 3226, 3256,
    3287, 3318, 3346, 3377, 3407, 3438, 3468, 3499, 3530, 3560, 3591, 3621,
    3652, 3683, 3712, 3743, 3773, 3804, 3834, 3865, 3896, 3926, 3957, 3987,
    4018, 4049, 4077, 4108, 4138, 4169, 4199, 4230, 4261, 4291, 4322, 4352,
    4383, 4414, 4442, 4473, 4503, 4534, 4564, 4595, 4626, 4656, 4687, 4717,
    4748, 4779, 4807, 4838, 4868, 4899, 4929, 4960, 4991, 5021, 5052, 5082,
    5113, 5144, 5173, 5204, 5234, 5265, 5295, 5326, 5357, 5387, 5418, 5448,
    5479, 5510, 5538, 5569, 5599, 5630, 5660, 5691, 5722, 5752, 5783, 5813,
    5844, 5875, 5903, 5934, 5964, 5995, 6025, 6056, 6087, 6117, 6148, 6178,
    6209, 6240, 6268, 6299, 6329, 6360, 6390, 6421, 6452, 6482, 6513, 6543,
    6574, 6605, 6634, 6665, 6695, 6726, 6756, 6787, 6818, 6848, 6879, 6909,
    6940, 6971, 6999, 7030, 7060, 7091, 7121, 7152, 7183, 7213, 7244, 7274,
    7305, 7336, 7364, 7395, 7425, 7456, 7486, 7517, 7548, 7578, 7609, 7639,
    7670, 7701, 7729, 7760, 7790, 7821, 7851, 7882, 7913, 7943, 7974, 8004,
    8035, 8066, 8095, 8126, 8156, 8187, 8217, 8248, 8279, 8309, 8340, 8370,
    8401, 8432, 8460, 8491, 8521, 8552, 8582, 8613, 8644, 8674, 8705, 8735,
    8766, 8797, 8825, 8856, 8886, 8917, 8947, 8978, 9009, 9039, 9070, 9100,
    9131, 9162, 9190, 9221, 9251, 9282, 9312, 9343, 9374, 9404, 9435, 9465,
    9496, 9527, 9556, 9587, 9617, 9648, 9678, 9709, 9740, 9770, 9801, 9831,
    9862, 9893, 9921, 9952, 9982, 10013, 10043, 10074, 10105, 10135, 10166, 10196,
    10227, 10258, 10286, 10317, 10347, 10378, 10408, 10439, 10470, 10500, 10531, 10561,
    10592, 10623, 10651, 10682, 10712, 10743, 10773, 10804, 10835, 10865, 10896, 10926,
    10957, 10988, 11017, 11048, 11078, 11109, 11139, 11170, 11201, 11231, 11262, 11292,
    11323, 11354, 11382, 11413, 11443, 11474, 11504, 11535, 11566, 11596, 11627, 11657,
    11688, 11719, 11747, 11778, 11808, 11839, 11869, 11900, 11931, 11961, 11992, 12022,
    12053, 12084, 12112, 12143, 12173, 12204, 12234, 12265, 12296, 12326, 12357, 12387,
    12418, 12449, 12478, 12509, 12539, 12570, 12600, 12631, 12662, 12692, 12723, 12753,
    12784, 12815, 12843, 12874, 12904, 12935, 12965, 12996, 13027, 13057, 13088, 13118,
    13149, 13180, 13208, 13239, 13269, 13300, 13330, 13361, 13392, 13422, 13453, 13483,
    13514, 13545, 13573, 13604, 13634, 13665, 13695, 13726, 13757, 13787, 13818, 13848,
    13879, 13910, 13939, 13970, 14000, 14031, 14061, 14092, 14123, 14153, 14184, 14214,
    14245, 14276, 14304, 14335, 14365, 14396, 14426, 14457, 14488, 14518, 14549, 14579,
    14610, 14641, 14669, 14700, 14730, 14761, 14791, 14822, 14853, 14883, 14914, 14944,
    14975, 15006, 15034, 15065, 15095, 15126, 15156, 15187, 15218, 15248, 15279, 15309,
    15340, 15371, 15400, 15431, 15461, 15492, 15522, 15553, 15584, 15614, 15645, 15675,
    15706, 15737, 15765, 15796, 15826, 15857, 15887, 15918, 15949, 15979, 16010, 16040,
    16071, 16102, 16130, 16161, 16191, 16222, 16252, 16283, 16314, 16344, 16375, 16405,
    16436, 16467, 16495, 16526, 16556, 16587, 16617, 16648, 16679, 16709, 16740, 16770,
    16801, 16832, 16861, 16892, 16922, 16953, 16983, 17014, 17045, 17075, 17106, 17136,
    17167, 17198, 17226, 17257, 17287, 17318, 17348, 17379, 17410, 17440, 17471, 17501,
    17532, 17563, 17591, 17622, 17652, 17683, 17713, 17744, 17775, 17805, 17836, 17866,
    17897, 17928, 17956, 17987, 18017, 18048, 18078, 18109, 18140, 18170, 18201, 18231,
    18262, 18293, 18322, 18353, 18383, 18414, 18444, 18475, 18506, 18536, 18567, 18597,
    18628, 18659, 18687, 18718, 18748, 18779, 18809, 18840, 18871, 18901, 18932, 18962,
    18993, 19024, 19052, 19083, 19113, 19144, 19174, 19205, 19236, 19266, 19297, 19327,
    19358, 19389, 19417, 19448, 19478, 19509, 19539, 19570, 19601, 19631, 19662, 19692,
    19723, 19754, 19783, 19814, 19844, 19875, 19905, 19936, 19967, 19997, 20028, 20058,
    20089, 20120, 20148, 20179, 20209, 20240, 20270, 20301, 20332, 20362, 20393, 20423,
    20454, 20485, 20513, 20544, 20574, 20605, 20635, 20666, 20697, 20727, 20758, 20788,
    20819, 20850, 20878, 20909, 20939, 20970, 21000, 21031, 21062, 21092, 21123, 21153,
    21184, 21215, 21244, 21275, 21305, 21336, 21366, 21397, 21428, 21458, 21489, 21519,
    21550, 21581, 21609, 21640, 21670, 21701, 21731, 21762, 21793, 21823, 21854, 21884,
    21915, 21946, 21974, 22005, 22035, 22066, 22096, 22127, 22158, 22188, 22219, 22249,
    22280, 22311, 22339, 22370, 22400, 22431, 22461, 22492, 22523, 22553, 22584, 22614,
    22645, 22676, 22705, 22736, 22766, 22797, 22827, 22858, 22889, 22919, 22950, 22980,
    23011, 23042, 23070, 23101, 23131, 23162, 23192, 23223, 23254, 23284, 23315, 23345,
    23376, 23407, 23435, 23466, 23496, 23527, 23557, 23588, 23619, 23649, 23680, 23710,
    23741, 23772, 23800, 23831, 23861, 23892, 23922, 23953, 23984, 24014, 24045, 24075,
    24106, 24137, 24166, 24197, 24227, 24258, 24288, 24319, 24350, 24380, 24411, 24441,
    24472, 24503, 24531, 24562, 24592, 24623, 24653, 24684, 24715, 24745, 24776, 24806,
    24837, 24868, 24896, 24927, 24957, 24988, 25018, 25049, 25080, 25110, 25141, 25171,
    25202, 25233, 25261, 25292, 25322, 25353, 25383, 25414, 25445, 25475, 25506, 25536,
    25567, 25598, 25627, 25658, 25688, 25719, 25749, 25780, 25811, 25841, 25872, 25902,
    25933, 25964, 25992, 26023, 26053, 26084, 26114, 26145, 26176, 26206, 26237, 26267,
    26298, 26329, 26357, 26388, 26418, 26449, 26479, 26510, 26541, 26571, 26602, 26632,
    26663, 26694, 26722, 26753, 26783, 26814, 26844, 26875, 26906, 26936, 26967, 26997,
    27028, 27059, 27088, 27119, 27149, 27180, 27210, 27241, 27272, 27302, 27333, 27363,
    27394, 27425, 27453, 27484, 27514, 27545, 27575, 27606, 27637, 27667, 27698, 27728,
    27759, 27790, 27818, 27849, 27879, 27910, 27940, 27971, 28002, 28032, 28063, 28093,
    28124, 28155, 28183, 28214, 28244, 28275, 28305, 28336, 28367, 28397, 28428, 28458,
    28489, 28520, 28549, 28580, 28610, 28641, 28671, 28702, 28733, 28763, 28794, 28824,
    28855, 28886, 28914, 28945, 28975, 29006, 29036, 29067, 29098, 29128, 29159, 29189,
    29220, 29251, 29279, 29310, 29340, 29371, 29401, 29432, 29463, 29493, 29524, 29554,
    29585, 29616, 29644, 29675, 29705, 29736, 29766, 29797, 29828, 29858, 29889, 29919,
    29950, 29981, 30010, 30041, 30071, 30102, 30132, 30163, 30194, 30224, 30255, 30285,
    30316, 30347, 30375, 30406, 30436, 30467, 30497, 30528, 30559, 30589, 30620, 30650,
    30681, 30712, 30740, 30771, 30801, 30832, 30862, 30893, 30924, 30954, 30985, 31015,
    31046, 31077, 31105, 31136, 31166, 31197, 31227, 31258, 31289, 31319, 31350, 31380,
    31411, 31442, 31471, 31502, 31532, 31563, 31593, 31624, 31655, 31685, 31716, 31746,
    31777, 31808, 31836, 31867, 31897, 31928, 31958, 31989, 32020, 32050, 32081, 32111,
    32142, 32173, 32201, 32232, 32262, 32293, 32323, 32354, 32385, 32415, 32446, 32476,
    32507, 32538, 32566, 32597, 32627, 32658, 32688, 32719, 32750, 32780, 32811, 32841,
    32872, 32903, 32932, 32963, 32993, 33024, 33054, 33085, 33116, 33146, 33177, 33207,
    33238, 33269, 33297, 33328, 33358, 33389, 33419, 33450, 33481, 33511, 33542, 33572,
    33603, 33634, 33662, 33693, 33723, 33754, 33784, 33815, 33846, 33876, 33907, 33937,
    33968, 33999, 34027, 34058, 34088, 34119, 34149, 34180, 34211, 34241, 34272, 34302,
    34333, 34364, 34393, 34424, 34454, 34485, 34515, 34546, 34577, 34607, 34638, 34668,
    34699, 34730, 34758, 34789, 34819, 34850, 34880, 34911, 34942, 34972, 35003, 35033,
    35064, 35095, 35123, 35154, 35184, 35215, 35245, 35276, 35307, 35337, 35368, 35398,
    35429, 35460, 35488, 35519, 35549, 35580, 35610, 35641, 35672, 35702, 35733, 35763,
    35794, 35825, 35854, 35885, 35915, 35946, 35976, 36007, 36038, 36068, 36099, 36129,
    36160, 36191, 36219, 36250, 36280, 36311, 36341, 36372, 36403, 36433, 36464, 36494,
    36525, 36556, 36584, 36615, 36645, 36676, 36706, 36737, 36768, 36798, 36829, 36859,
    36890, 36921, 36949, 36980, 37010, 37041, 37071, 37102, 37133, 37163, 37194, 37224,
    37255, 37286, 37315, 37346, 37376, 37407, 37437, 37468, 37499, 37529, 37560, 37590,
    37621, 37652, 37680, 37711, 37741, 37772, 37802, 37833, 37864, 37894, 37925, 37955,
    37986, 38017, 38045, 38076, 38106, 38137, 38167, 38198, 38229, 38259, 38290, 38320,
    38351, 38382, 38410, 38441, 38471, 38502, 38532, 38563, 38594, 38624, 38655, 38685};

#define ARR IDATE_BY_YEARMONTH_SINCE_1948_JAN_1ST

unsigned int bsearch_nrst(register int x, unsigned int lwr, unsigned int upr) {
  unsigned int d = upr - lwr;
  if (d <= 1) {
    return lwr;
  }
  unsigned int mid = (upr + lwr) >> 1;
  if (x >= ARR[mid]) {
    return bsearch_nrst(x, mid, upr);
  } else {
    return bsearch_nrst(x, lwr, mid);
  }
}

static YearMonth iym(int year, int month) {
  YearMonth O = { .year = year, .month = month};
  return O;
}

// position of IDate (as integer) within the array
// (i.e. the yearmonth since 1948-01, rounded down)
unsigned int p_search(int x) {
  if (x < 0) {
    if (x < -8005) {
      return 0;
    }
    return bsearch_nrst(x, 1, 265);

  }

  if (x < 15706) {
    // 2013
    // p = bsearch_nrst(x, 263, 781);
    return bsearch_nrst(x, 263, 781);
  } else {
    return bsearch_nrst(x, 780, 1535);
  }
}

YearMonth idate2YearMonth(int x) {
  unsigned int p = p_search(x);
  // p = bsearch_nrst(x, 0, 1535);
  return iym(p / 12, (p % 12) + 1);
}


uint16_t year(int x) {
  uint16_t p = p_search(x);
  return p / 12;
}

uint16_t year2(int x, unsigned int lwr, unsigned int upr) {
  uint16_t p = bsearch_nrst(x, lwr, upr);
  return p / 12;
}

SEXP C_Year(SEXP IDates, SEXP nthreads) {
  if (!isInteger(IDates)) {
    return R_NilValue;
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(IDates);
  const int * xp = INTEGER(IDates);
  int min_idate = xp[0];
  int max_idate = xp[0];
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : min_idate) reduction(max : max_idate)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    int xpi = xp[i];
    min_idate = (min_idate < xpi) ? min_idate : xpi;
    max_idate = (max_idate > xpi) ? max_idate : xpi;
  }
  if (min_idate < MIN_IDATE || max_idate > MAX_IDATE) {
    return R_NilValue; // # nocov
  }

  const unsigned int p_min_idate = p_search(min_idate);
  const unsigned int p_max_idate = p_search(max_idate);

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    ansp[i] = year2(xp[i], p_min_idate, p_max_idate) + MIN_YEAR;
  })
  UNPROTECT(1);
  return ans;


}
static int string102year(const char * x) {
  switch(x[0]) {
  case '1':
    if (x[1] == '9' && isdigit(x[2]) && isdigit(x[3])) {
      return 1900 + 10 * (x[2] - '0') + (x[3] - '0');
    } else {
      return NA_INTEGER;
    }
    break;
  case '2':
    if (x[1] == '0' && isdigit(x[2]) && isdigit(x[3])) {
      return 2000 + 10 * (x[2] - '0') + (x[3] - '0');
    } else {
      return NA_INTEGER;
    }
    break;
  }
  return NA_INTEGER;
}

static int string102month(const char * x) {
  switch(x[5]) {
  case '0':
    return isdigit(x[6]) ? x[6] - '0' : NA_INTEGER;
  case '1':
    switch(x[6]) {
    case '0':
      return 10;
    case '1':
      return 11;
    case '2':
      return 12;
    }
  }
  return NA_INTEGER;
}

static int string2fy(const char * x) {
  int yy = string102year(x);
  return yy + 1;
}

// ignores date
SEXP C_fastIDate(SEXP x, SEXP IncludeDay, SEXP Check, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isString(x)) {
    error("Expected a STRSXP.");
  }
  const bool incl_day = asLogical(IncludeDay);
  const int check = asInteger(Check);
  const SEXP * xp = STRING_PTR(x);
  R_xlen_t N = xlength(x);

  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    int n = length(xp[i]);
    const char * xi = CHAR(xp[i]);
    if (n != 10) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    ansp[i] = 0;
    int year_i = string102year(xi);
    unsigned int month_i = string102month(xi);
    if (year_i < 1948 || year_i > 2075 || month_i > 12) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    ansp[i] = ARR[12 * (year_i - 1948) + (month_i - 1)];
    if (incl_day) {
      ansp[i] += 10 * (xi[8] - '0') + (xi[9] - '0') - 1;
    }
  })
  UNPROTECT(1);
  return ans;
}

unsigned int p_search_string10(const char * x) {
  unsigned int m = string102month(x);
  unsigned int y = string102year(x);
  y -= MIN_YEAR;
  if (y > 127 && m > 12) {
    y = 127;
  }
  return 12 * y + m - 1;
}

unsigned int p_search_string7_unsafe(const char * x) {
  int y = string2fy(x);
  y -= MIN_YEAR;
  return 12 * y + 18; // FY
}








