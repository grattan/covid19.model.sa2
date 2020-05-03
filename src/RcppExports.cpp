// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// distr2status
IntegerVector distr2status(int N, int dead, int healed, int active, int critical);
RcppExport SEXP _covid19_model_sa2_distr2status(SEXP NSEXP, SEXP deadSEXP, SEXP healedSEXP, SEXP activeSEXP, SEXP criticalSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< int >::type dead(deadSEXP);
    Rcpp::traits::input_parameter< int >::type healed(healedSEXP);
    Rcpp::traits::input_parameter< int >::type active(activeSEXP);
    Rcpp::traits::input_parameter< int >::type critical(criticalSEXP);
    rcpp_result_gen = Rcpp::wrap(distr2status(N, dead, healed, active, critical));
    return rcpp_result_gen;
END_RCPP
}
// status_killed
int status_killed();
RcppExport SEXP _covid19_model_sa2_status_killed() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(status_killed());
    return rcpp_result_gen;
END_RCPP
}
// status_healed
int status_healed();
RcppExport SEXP _covid19_model_sa2_status_healed() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(status_healed());
    return rcpp_result_gen;
END_RCPP
}
// status_suscep
int status_suscep();
RcppExport SEXP _covid19_model_sa2_status_suscep() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(status_suscep());
    return rcpp_result_gen;
END_RCPP
}
// status_nosymp
int status_nosymp();
RcppExport SEXP _covid19_model_sa2_status_nosymp() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(status_nosymp());
    return rcpp_result_gen;
END_RCPP
}
// status_insymp
int status_insymp();
RcppExport SEXP _covid19_model_sa2_status_insymp() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(status_insymp());
    return rcpp_result_gen;
END_RCPP
}
// status_critic
int status_critic();
RcppExport SEXP _covid19_model_sa2_status_critic() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(status_critic());
    return rcpp_result_gen;
END_RCPP
}
// supermarket_weekday_hrs
int supermarket_weekday_hrs();
RcppExport SEXP _covid19_model_sa2_supermarket_weekday_hrs() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(supermarket_weekday_hrs());
    return rcpp_result_gen;
END_RCPP
}
// supermarket_weekend_hrs
int supermarket_weekend_hrs();
RcppExport SEXP _covid19_model_sa2_supermarket_weekend_hrs() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    rcpp_result_gen = Rcpp::wrap(supermarket_weekend_hrs());
    return rcpp_result_gen;
END_RCPP
}
// do_max_par_int
int do_max_par_int(IntegerVector x, int nThread);
RcppExport SEXP _covid19_model_sa2_do_max_par_int(SEXP xSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_max_par_int(x, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_rep
IntegerVector do_rep(IntegerVector r, int nThread);
RcppExport SEXP _covid19_model_sa2_do_rep(SEXP rSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_rep(r, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_au_simulate
List do_au_simulate(IntegerVector Status, IntegerVector InfectedOn, IntegerVector State, IntegerVector SA2, IntegerVector hid, IntegerVector seqN, IntegerVector HouseholdSize, IntegerVector Age, IntegerVector School, IntegerVector DZN, IntegerVector wid, IntegerVector nColleagues, IntegerVector PlaceTypeBySA2, IntegerVector LabourForceStatus, IntegerVector Resistance, List Policy, List nPlacesByDestType, List FreqsByDestType, List Epi, /* Epidemiological parameters */                     IntegerVector nSupermarketsAvbl, IntegerVector SupermarketTypical, int yday_start, int days_to_sim, int N, bool display_progress, bool by_state, int console_width, int optionz, int nThread);
RcppExport SEXP _covid19_model_sa2_do_au_simulate(SEXP StatusSEXP, SEXP InfectedOnSEXP, SEXP StateSEXP, SEXP SA2SEXP, SEXP hidSEXP, SEXP seqNSEXP, SEXP HouseholdSizeSEXP, SEXP AgeSEXP, SEXP SchoolSEXP, SEXP DZNSEXP, SEXP widSEXP, SEXP nColleaguesSEXP, SEXP PlaceTypeBySA2SEXP, SEXP LabourForceStatusSEXP, SEXP ResistanceSEXP, SEXP PolicySEXP, SEXP nPlacesByDestTypeSEXP, SEXP FreqsByDestTypeSEXP, SEXP EpiSEXP, SEXP nSupermarketsAvblSEXP, SEXP SupermarketTypicalSEXP, SEXP yday_startSEXP, SEXP days_to_simSEXP, SEXP NSEXP, SEXP display_progressSEXP, SEXP by_stateSEXP, SEXP console_widthSEXP, SEXP optionzSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type Status(StatusSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type InfectedOn(InfectedOnSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type State(StateSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type SA2(SA2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type hid(hidSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type seqN(seqNSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type HouseholdSize(HouseholdSizeSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type Age(AgeSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type School(SchoolSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type DZN(DZNSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type wid(widSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type nColleagues(nColleaguesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type PlaceTypeBySA2(PlaceTypeBySA2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type LabourForceStatus(LabourForceStatusSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type Resistance(ResistanceSEXP);
    Rcpp::traits::input_parameter< List >::type Policy(PolicySEXP);
    Rcpp::traits::input_parameter< List >::type nPlacesByDestType(nPlacesByDestTypeSEXP);
    Rcpp::traits::input_parameter< List >::type FreqsByDestType(FreqsByDestTypeSEXP);
    Rcpp::traits::input_parameter< List >::type Epi(EpiSEXP);
    Rcpp::traits::input_parameter< /* Epidemiological parameters */                     IntegerVector >::type nSupermarketsAvbl(nSupermarketsAvblSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type SupermarketTypical(SupermarketTypicalSEXP);
    Rcpp::traits::input_parameter< int >::type yday_start(yday_startSEXP);
    Rcpp::traits::input_parameter< int >::type days_to_sim(days_to_simSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    Rcpp::traits::input_parameter< bool >::type by_state(by_stateSEXP);
    Rcpp::traits::input_parameter< int >::type console_width(console_widthSEXP);
    Rcpp::traits::input_parameter< int >::type optionz(optionzSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_au_simulate(Status, InfectedOn, State, SA2, hid, seqN, HouseholdSize, Age, School, DZN, wid, nColleagues, PlaceTypeBySA2, LabourForceStatus, Resistance, Policy, nPlacesByDestType, FreqsByDestType, Epi, nSupermarketsAvbl, SupermarketTypical, yday_start, days_to_sim, N, display_progress, by_state, console_width, optionz, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_exp_dbl2int
IntegerVector do_exp_dbl2int(DoubleVector x, int nThread);
RcppExport SEXP _covid19_model_sa2_do_exp_dbl2int(SEXP xSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DoubleVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_exp_dbl2int(x, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_lag_int
IntegerVector do_lag_int(IntegerVector s, int nThread);
RcppExport SEXP _covid19_model_sa2_do_lag_int(SEXP sSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_lag_int(s, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_seqN_N
List do_seqN_N(IntegerVector hid, IntegerVector pid, bool check_hid_sorted);
RcppExport SEXP _covid19_model_sa2_do_seqN_N(SEXP hidSEXP, SEXP pidSEXP, SEXP check_hid_sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type hid(hidSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type pid(pidSEXP);
    Rcpp::traits::input_parameter< bool >::type check_hid_sorted(check_hid_sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(do_seqN_N(hid, pid, check_hid_sorted));
    return rcpp_result_gen;
END_RCPP
}
// get_nColleagues
IntegerVector get_nColleagues(int nr, int N, IntegerVector LabourForceStatus, int nThread, int c_d, double beta, double mu, double sigma);
RcppExport SEXP _covid19_model_sa2_get_nColleagues(SEXP nrSEXP, SEXP NSEXP, SEXP LabourForceStatusSEXP, SEXP nThreadSEXP, SEXP c_dSEXP, SEXP betaSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nr(nrSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type LabourForceStatus(LabourForceStatusSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    Rcpp::traits::input_parameter< int >::type c_d(c_dSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(get_nColleagues(nr, N, LabourForceStatus, nThread, c_d, beta, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}
// do_workplaces
List do_workplaces(List AusByDZN, int nThread, int c_d, double beta, double mu, double sigma);
RcppExport SEXP _covid19_model_sa2_do_workplaces(SEXP AusByDZNSEXP, SEXP nThreadSEXP, SEXP c_dSEXP, SEXP betaSEXP, SEXP muSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type AusByDZN(AusByDZNSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    Rcpp::traits::input_parameter< int >::type c_d(c_dSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type mu(muSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(do_workplaces(AusByDZN, nThread, c_d, beta, mu, sigma));
    return rcpp_result_gen;
END_RCPP
}
// testOpenmp
NumericVector testOpenmp(NumericVector x, int nThread);
RcppExport SEXP _covid19_model_sa2_testOpenmp(SEXP xSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(testOpenmp(x, nThread));
    return rcpp_result_gen;
END_RCPP
}
// postcode_to_sa2_sorted
IntegerVector postcode_to_sa2_sorted(IntegerVector postcode, IntegerVector POSTCODE, IntegerVector SA2_MAINCODE);
RcppExport SEXP _covid19_model_sa2_postcode_to_sa2_sorted(SEXP postcodeSEXP, SEXP POSTCODESEXP, SEXP SA2_MAINCODESEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type postcode(postcodeSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type POSTCODE(POSTCODESEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type SA2_MAINCODE(SA2_MAINCODESEXP);
    rcpp_result_gen = Rcpp::wrap(postcode_to_sa2_sorted(postcode, POSTCODE, SA2_MAINCODE));
    return rcpp_result_gen;
END_RCPP
}
// punif_int
IntegerVector punif_int(int n, int a, int b, int nThread);
RcppExport SEXP _covid19_model_sa2_punif_int(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(punif_int(n, a, b, nThread));
    return rcpp_result_gen;
END_RCPP
}
// dqsample_int2
IntegerVector dqsample_int2(int m, int n);
RcppExport SEXP _covid19_model_sa2_dqsample_int2(SEXP mSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(dqsample_int2(m, n));
    return rcpp_result_gen;
END_RCPP
}
// prlnorm_dbl
DoubleVector prlnorm_dbl(int n, double a, double b, int nThread);
RcppExport SEXP _covid19_model_sa2_prlnorm_dbl(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(prlnorm_dbl(n, a, b, nThread));
    return rcpp_result_gen;
END_RCPP
}
// prlnorm_int
IntegerVector prlnorm_int(int n, double a, double b, int nThread);
RcppExport SEXP _covid19_model_sa2_prlnorm_int(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(prlnorm_int(n, a, b, nThread));
    return rcpp_result_gen;
END_RCPP
}
// prcauchy
DoubleVector prcauchy(int n, double a, double b, int nThread);
RcppExport SEXP _covid19_model_sa2_prcauchy(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(prcauchy(n, a, b, nThread));
    return rcpp_result_gen;
END_RCPP
}
// lemire_rand
IntegerVector lemire_rand(int n, int d, int s32, int nThread, unsigned int q2);
RcppExport SEXP _covid19_model_sa2_lemire_rand(SEXP nSEXP, SEXP dSEXP, SEXP s32SEXP, SEXP nThreadSEXP, SEXP q2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type d(dSEXP);
    Rcpp::traits::input_parameter< int >::type s32(s32SEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type q2(q2SEXP);
    rcpp_result_gen = Rcpp::wrap(lemire_rand(n, d, s32, nThread, q2));
    return rcpp_result_gen;
END_RCPP
}
// short_sa2
int short_sa2(int sa2);
RcppExport SEXP _covid19_model_sa2_short_sa2(SEXP sa2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< int >::type sa2(sa2SEXP);
    rcpp_result_gen = Rcpp::wrap(short_sa2(sa2));
    return rcpp_result_gen;
END_RCPP
}
// shorten_sa2s_ordered
IntegerVector shorten_sa2s_ordered(IntegerVector SA2);
RcppExport SEXP _covid19_model_sa2_shorten_sa2s_ordered(SEXP SA2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type SA2(SA2SEXP);
    rcpp_result_gen = Rcpp::wrap(shorten_sa2s_ordered(SA2));
    return rcpp_result_gen;
END_RCPP
}
// do_minmax_par
IntegerVector do_minmax_par(IntegerVector x, int nThread);
RcppExport SEXP _covid19_model_sa2_do_minmax_par(SEXP xSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_minmax_par(x, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_ModuloIndex
IntegerVector do_ModuloIndex(IntegerVector x, int mod, int max, int nThread);
RcppExport SEXP _covid19_model_sa2_do_ModuloIndex(SEXP xSEXP, SEXP modSEXP, SEXP maxSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type mod(modSEXP);
    Rcpp::traits::input_parameter< int >::type max(maxSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_ModuloIndex(x, mod, max, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_modulo_d
IntegerVector do_modulo_d(IntegerVector x, int m, int divisor, int nThread);
RcppExport SEXP _covid19_model_sa2_do_modulo_d(SEXP xSEXP, SEXP mSEXP, SEXP divisorSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type divisor(divisorSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_modulo_d(x, m, divisor, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_modulo_16
IntegerVector do_modulo_16(IntegerVector x, int nThread);
RcppExport SEXP _covid19_model_sa2_do_modulo_16(SEXP xSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_modulo_16(x, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_lag_in_place
IntegerVector do_lag_in_place(IntegerVector x);
RcppExport SEXP _covid19_model_sa2_do_lag_in_place(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(do_lag_in_place(x));
    return rcpp_result_gen;
END_RCPP
}
// do_pminCppp
IntegerVector do_pminCppp(IntegerVector x, int a, int nThread);
RcppExport SEXP _covid19_model_sa2_do_pminCppp(SEXP xSEXP, SEXP aSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_pminCppp(x, a, nThread));
    return rcpp_result_gen;
END_RCPP
}
// test_threadsafe_mod
IntegerVector test_threadsafe_mod(IntegerVector x, IntegerVector y, int nThread);
RcppExport SEXP _covid19_model_sa2_test_threadsafe_mod(SEXP xSEXP, SEXP ySEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(test_threadsafe_mod(x, y, nThread));
    return rcpp_result_gen;
END_RCPP
}
// do_is_unsorted_pint
bool do_is_unsorted_pint(IntegerVector x, int nThread);
RcppExport SEXP _covid19_model_sa2_do_is_unsorted_pint(SEXP xSEXP, SEXP nThreadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nThread(nThreadSEXP);
    rcpp_result_gen = Rcpp::wrap(do_is_unsorted_pint(x, nThread));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_covid19_model_sa2_distr2status", (DL_FUNC) &_covid19_model_sa2_distr2status, 5},
    {"_covid19_model_sa2_status_killed", (DL_FUNC) &_covid19_model_sa2_status_killed, 0},
    {"_covid19_model_sa2_status_healed", (DL_FUNC) &_covid19_model_sa2_status_healed, 0},
    {"_covid19_model_sa2_status_suscep", (DL_FUNC) &_covid19_model_sa2_status_suscep, 0},
    {"_covid19_model_sa2_status_nosymp", (DL_FUNC) &_covid19_model_sa2_status_nosymp, 0},
    {"_covid19_model_sa2_status_insymp", (DL_FUNC) &_covid19_model_sa2_status_insymp, 0},
    {"_covid19_model_sa2_status_critic", (DL_FUNC) &_covid19_model_sa2_status_critic, 0},
    {"_covid19_model_sa2_supermarket_weekday_hrs", (DL_FUNC) &_covid19_model_sa2_supermarket_weekday_hrs, 0},
    {"_covid19_model_sa2_supermarket_weekend_hrs", (DL_FUNC) &_covid19_model_sa2_supermarket_weekend_hrs, 0},
    {"_covid19_model_sa2_do_max_par_int", (DL_FUNC) &_covid19_model_sa2_do_max_par_int, 2},
    {"_covid19_model_sa2_do_rep", (DL_FUNC) &_covid19_model_sa2_do_rep, 2},
    {"_covid19_model_sa2_do_au_simulate", (DL_FUNC) &_covid19_model_sa2_do_au_simulate, 29},
    {"_covid19_model_sa2_do_exp_dbl2int", (DL_FUNC) &_covid19_model_sa2_do_exp_dbl2int, 2},
    {"_covid19_model_sa2_do_lag_int", (DL_FUNC) &_covid19_model_sa2_do_lag_int, 2},
    {"_covid19_model_sa2_do_seqN_N", (DL_FUNC) &_covid19_model_sa2_do_seqN_N, 3},
    {"_covid19_model_sa2_get_nColleagues", (DL_FUNC) &_covid19_model_sa2_get_nColleagues, 8},
    {"_covid19_model_sa2_do_workplaces", (DL_FUNC) &_covid19_model_sa2_do_workplaces, 6},
    {"_covid19_model_sa2_testOpenmp", (DL_FUNC) &_covid19_model_sa2_testOpenmp, 2},
    {"_covid19_model_sa2_postcode_to_sa2_sorted", (DL_FUNC) &_covid19_model_sa2_postcode_to_sa2_sorted, 3},
    {"_covid19_model_sa2_punif_int", (DL_FUNC) &_covid19_model_sa2_punif_int, 4},
    {"_covid19_model_sa2_dqsample_int2", (DL_FUNC) &_covid19_model_sa2_dqsample_int2, 2},
    {"_covid19_model_sa2_prlnorm_dbl", (DL_FUNC) &_covid19_model_sa2_prlnorm_dbl, 4},
    {"_covid19_model_sa2_prlnorm_int", (DL_FUNC) &_covid19_model_sa2_prlnorm_int, 4},
    {"_covid19_model_sa2_prcauchy", (DL_FUNC) &_covid19_model_sa2_prcauchy, 4},
    {"_covid19_model_sa2_lemire_rand", (DL_FUNC) &_covid19_model_sa2_lemire_rand, 5},
    {"_covid19_model_sa2_short_sa2", (DL_FUNC) &_covid19_model_sa2_short_sa2, 1},
    {"_covid19_model_sa2_shorten_sa2s_ordered", (DL_FUNC) &_covid19_model_sa2_shorten_sa2s_ordered, 1},
    {"_covid19_model_sa2_do_minmax_par", (DL_FUNC) &_covid19_model_sa2_do_minmax_par, 2},
    {"_covid19_model_sa2_do_ModuloIndex", (DL_FUNC) &_covid19_model_sa2_do_ModuloIndex, 4},
    {"_covid19_model_sa2_do_modulo_d", (DL_FUNC) &_covid19_model_sa2_do_modulo_d, 4},
    {"_covid19_model_sa2_do_modulo_16", (DL_FUNC) &_covid19_model_sa2_do_modulo_16, 2},
    {"_covid19_model_sa2_do_lag_in_place", (DL_FUNC) &_covid19_model_sa2_do_lag_in_place, 1},
    {"_covid19_model_sa2_do_pminCppp", (DL_FUNC) &_covid19_model_sa2_do_pminCppp, 3},
    {"_covid19_model_sa2_test_threadsafe_mod", (DL_FUNC) &_covid19_model_sa2_test_threadsafe_mod, 3},
    {"_covid19_model_sa2_do_is_unsorted_pint", (DL_FUNC) &_covid19_model_sa2_do_is_unsorted_pint, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_covid19_model_sa2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
