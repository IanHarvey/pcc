#ifndef FOREACH_H
#define FOREACH_H

#define _CONCAT(a, b) a##b
#define CONCAT(a, b) _CONCAT(a, b)
#define CONCAT2(a, b) _CONCAT(a, b)

#define _CONCAT3(a, b, c) a##b##c
#define CONCAT3(a, b, c) _CONCAT3(a, b, c)

#define _CONCAT4(a, b, c, d) a##b##c##d
#define CONCAT4(a, b, c, d) _CONCAT4(a, b, c, d)

#define EXPAND(x) x

#define _COUNT_ARGS( _1,  _2,  _3,  _4,  _5,  _6,  _7,  _8,  _9, _10, \
                    _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, \
                    _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, \
                    _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, \
                    _41, _42, _43, _44, _45, _46, _47, _48, _49, _50, \
                    _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, \
                    _61, _62, _63, _64, N, ...) N
#define COUNT_ARGS(...) \
        _COUNT_ARGS(__VA_ARGS__,        64, 63, 62, 61, 60, \
                    59, 58, 57, 56, 55, 54, 53, 52, 51, 50, \
                    49, 48, 47, 46, 45, 44, 43, 42, 41, 40, \
                    39, 38, 37, 36, 35, 34, 33, 32, 31, 30, \
                    29, 28, 27, 26, 25, 24, 23, 22, 21, 20, \
                    19, 18, 17, 16, 15, 14, 13, 12, 11, 10, \
                     9,  8,  7,  6,  5,  4,  3,  2,  1,  0)

#define FOR_EACH_1(func, x) \
        func(x)
#define FOR_EACH_2(func, x, ...) \
        func(x), EXPAND(FOR_EACH_1(func, __VA_ARGS__))
#define FOR_EACH_3(func, x, ...) \
        func(x), EXPAND(FOR_EACH_2(func, __VA_ARGS__))
#define FOR_EACH_4(func, x, ...) \
        func(x), EXPAND(FOR_EACH_3(func, __VA_ARGS__))
#define FOR_EACH_5(func, x, ...) \
        func(x), EXPAND(FOR_EACH_4(func, __VA_ARGS__))
#define FOR_EACH_6(func, x, ...) \
        func(x), EXPAND(FOR_EACH_5(func, __VA_ARGS__))
#define FOR_EACH_7(func, x, ...) \
        func(x), EXPAND(FOR_EACH_6(func, __VA_ARGS__))
#define FOR_EACH_8(func, x, ...) \
        func(x), EXPAND(FOR_EACH_7(func, __VA_ARGS__))
#define FOR_EACH_9(func, x, ...) \
        func(x), EXPAND(FOR_EACH_8(func, __VA_ARGS__))
#define FOR_EACH_10(func, x, ...) \
        func(x), EXPAND(FOR_EACH_9(func, __VA_ARGS__))
#define FOR_EACH_11(func, x, ...) \
        func(x), EXPAND(FOR_EACH_10(func, __VA_ARGS__))
#define FOR_EACH_12(func, x, ...) \
        func(x), EXPAND(FOR_EACH_11(func, __VA_ARGS__))
#define FOR_EACH_13(func, x, ...) \
        func(x), EXPAND(FOR_EACH_12(func, __VA_ARGS__))
#define FOR_EACH_14(func, x, ...) \
        func(x), EXPAND(FOR_EACH_13(func, __VA_ARGS__))
#define FOR_EACH_15(func, x, ...) \
        func(x), EXPAND(FOR_EACH_14(func, __VA_ARGS__))
#define FOR_EACH_16(func, x, ...) \
        func(x), EXPAND(FOR_EACH_15(func, __VA_ARGS__))
#define FOR_EACH_17(func, x, ...) \
        func(x), EXPAND(FOR_EACH_16(func, __VA_ARGS__))
#define FOR_EACH_18(func, x, ...) \
        func(x), EXPAND(FOR_EACH_17(func, __VA_ARGS__))
#define FOR_EACH_19(func, x, ...) \
        func(x), EXPAND(FOR_EACH_18(func, __VA_ARGS__))
#define FOR_EACH_20(func, x, ...) \
        func(x), EXPAND(FOR_EACH_19(func, __VA_ARGS__))
#define FOR_EACH_21(func, x, ...) \
        func(x), EXPAND(FOR_EACH_20(func, __VA_ARGS__))
#define FOR_EACH_22(func, x, ...) \
        func(x), EXPAND(FOR_EACH_21(func, __VA_ARGS__))
#define FOR_EACH_23(func, x, ...) \
        func(x), EXPAND(FOR_EACH_22(func, __VA_ARGS__))
#define FOR_EACH_24(func, x, ...) \
        func(x), EXPAND(FOR_EACH_23(func, __VA_ARGS__))
#define FOR_EACH_25(func, x, ...) \
        func(x), EXPAND(FOR_EACH_24(func, __VA_ARGS__))
#define FOR_EACH_26(func, x, ...) \
        func(x), EXPAND(FOR_EACH_25(func, __VA_ARGS__))
#define FOR_EACH_27(func, x, ...) \
        func(x), EXPAND(FOR_EACH_26(func, __VA_ARGS__))
#define FOR_EACH_28(func, x, ...) \
        func(x), EXPAND(FOR_EACH_27(func, __VA_ARGS__))
#define FOR_EACH_29(func, x, ...) \
        func(x), EXPAND(FOR_EACH_28(func, __VA_ARGS__))
#define FOR_EACH_30(func, x, ...) \
        func(x), EXPAND(FOR_EACH_29(func, __VA_ARGS__))
#define FOR_EACH_31(func, x, ...) \
        func(x), EXPAND(FOR_EACH_30(func, __VA_ARGS__))
#define FOR_EACH_32(func, x, ...) \
        func(x), EXPAND(FOR_EACH_31(func, __VA_ARGS__))
#define FOR_EACH_33(func, x, ...) \
        func(x), EXPAND(FOR_EACH_32(func, __VA_ARGS__))
#define FOR_EACH_34(func, x, ...) \
        func(x), EXPAND(FOR_EACH_33(func, __VA_ARGS__))
#define FOR_EACH_35(func, x, ...) \
        func(x), EXPAND(FOR_EACH_34(func, __VA_ARGS__))
#define FOR_EACH_36(func, x, ...) \
        func(x), EXPAND(FOR_EACH_35(func, __VA_ARGS__))
#define FOR_EACH_37(func, x, ...) \
        func(x), EXPAND(FOR_EACH_36(func, __VA_ARGS__))
#define FOR_EACH_38(func, x, ...) \
        func(x), EXPAND(FOR_EACH_37(func, __VA_ARGS__))
#define FOR_EACH_39(func, x, ...) \
        func(x), EXPAND(FOR_EACH_38(func, __VA_ARGS__))
#define FOR_EACH_40(func, x, ...) \
        func(x), EXPAND(FOR_EACH_39(func, __VA_ARGS__))
#define FOR_EACH_41(func, x, ...) \
        func(x), EXPAND(FOR_EACH_40(func, __VA_ARGS__))
#define FOR_EACH_42(func, x, ...) \
        func(x), EXPAND(FOR_EACH_41(func, __VA_ARGS__))
#define FOR_EACH_43(func, x, ...) \
        func(x), EXPAND(FOR_EACH_42(func, __VA_ARGS__))
#define FOR_EACH_44(func, x, ...) \
        func(x), EXPAND(FOR_EACH_43(func, __VA_ARGS__))
#define FOR_EACH_45(func, x, ...) \
        func(x), EXPAND(FOR_EACH_44(func, __VA_ARGS__))
#define FOR_EACH_46(func, x, ...) \
        func(x), EXPAND(FOR_EACH_45(func, __VA_ARGS__))
#define FOR_EACH_47(func, x, ...) \
        func(x), EXPAND(FOR_EACH_46(func, __VA_ARGS__))
#define FOR_EACH_48(func, x, ...) \
        func(x), EXPAND(FOR_EACH_47(func, __VA_ARGS__))
#define FOR_EACH_49(func, x, ...) \
        func(x), EXPAND(FOR_EACH_48(func, __VA_ARGS__))
#define FOR_EACH_50(func, x, ...) \
        func(x), EXPAND(FOR_EACH_49(func, __VA_ARGS__))
#define FOR_EACH_51(func, x, ...) \
        func(x), EXPAND(FOR_EACH_50(func, __VA_ARGS__))
#define FOR_EACH_52(func, x, ...) \
        func(x), EXPAND(FOR_EACH_51(func, __VA_ARGS__))
#define FOR_EACH_53(func, x, ...) \
        func(x), EXPAND(FOR_EACH_52(func, __VA_ARGS__))
#define FOR_EACH_54(func, x, ...) \
        func(x), EXPAND(FOR_EACH_53(func, __VA_ARGS__))
#define FOR_EACH_55(func, x, ...) \
        func(x), EXPAND(FOR_EACH_54(func, __VA_ARGS__))
#define FOR_EACH_56(func, x, ...) \
        func(x), EXPAND(FOR_EACH_55(func, __VA_ARGS__))
#define FOR_EACH_57(func, x, ...) \
        func(x), EXPAND(FOR_EACH_56(func, __VA_ARGS__))
#define FOR_EACH_58(func, x, ...) \
        func(x), EXPAND(FOR_EACH_57(func, __VA_ARGS__))
#define FOR_EACH_59(func, x, ...) \
        func(x), EXPAND(FOR_EACH_58(func, __VA_ARGS__))
#define FOR_EACH_60(func, x, ...) \
        func(x), EXPAND(FOR_EACH_59(func, __VA_ARGS__))
#define FOR_EACH_61(func, x, ...) \
        func(x), EXPAND(FOR_EACH_60(func, __VA_ARGS__))
#define FOR_EACH_62(func, x, ...) \
        func(x), EXPAND(FOR_EACH_61(func, __VA_ARGS__))
#define FOR_EACH_63(func, x, ...) \
        func(x), EXPAND(FOR_EACH_62(func, __VA_ARGS__))
#define FOR_EACH_64(func, x, ...) \
        func(x), EXPAND(FOR_EACH_63(func, __VA_ARGS__))
#define FOR_EACH(func, ...) \
        EXPAND(CONCAT(FOR_EACH_, COUNT_ARGS(__VA_ARGS__))(func, __VA_ARGS__))

#endif

#define SQ(x) (x * x)

int squares[] = FOR_EACH(SQ, 1, 2, 3, 4, 5, 6, 7, 8);
