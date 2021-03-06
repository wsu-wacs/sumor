// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// getCO2
List getCO2(const int start, const int duration);
RcppExport SEXP _sumor_getCO2(SEXP startSEXP, SEXP durationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type start(startSEXP);
    Rcpp::traits::input_parameter< const int >::type duration(durationSEXP);
    rcpp_result_gen = Rcpp::wrap(getCO2(start, duration));
    return rcpp_result_gen;
END_RCPP
}
// getJunctions_int
List getJunctions_int();
RcppExport SEXP _sumor_getJunctions_int() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getJunctions_int());
    return rcpp_result_gen;
END_RCPP
}
// getPedestrianPositions
List getPedestrianPositions(const int start, const int duration);
RcppExport SEXP _sumor_getPedestrianPositions(SEXP startSEXP, SEXP durationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type start(startSEXP);
    Rcpp::traits::input_parameter< const int >::type duration(durationSEXP);
    rcpp_result_gen = Rcpp::wrap(getPedestrianPositions(start, duration));
    return rcpp_result_gen;
END_RCPP
}
// getVehiclePositions
List getVehiclePositions(const int start, const int duration);
RcppExport SEXP _sumor_getVehiclePositions(SEXP startSEXP, SEXP durationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type start(startSEXP);
    Rcpp::traits::input_parameter< const int >::type duration(durationSEXP);
    rcpp_result_gen = Rcpp::wrap(getVehiclePositions(start, duration));
    return rcpp_result_gen;
END_RCPP
}
// getVehicleFuelConsumption
List getVehicleFuelConsumption(const int start, const int duration);
RcppExport SEXP _sumor_getVehicleFuelConsumption(SEXP startSEXP, SEXP durationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type start(startSEXP);
    Rcpp::traits::input_parameter< const int >::type duration(durationSEXP);
    rcpp_result_gen = Rcpp::wrap(getVehicleFuelConsumption(start, duration));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sumor_getCO2", (DL_FUNC) &_sumor_getCO2, 2},
    {"_sumor_getJunctions_int", (DL_FUNC) &_sumor_getJunctions_int, 0},
    {"_sumor_getPedestrianPositions", (DL_FUNC) &_sumor_getPedestrianPositions, 2},
    {"_sumor_getVehiclePositions", (DL_FUNC) &_sumor_getVehiclePositions, 2},
    {"_sumor_getVehicleFuelConsumption", (DL_FUNC) &_sumor_getVehicleFuelConsumption, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_sumor(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
