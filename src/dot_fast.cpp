#include <Rcpp.h>
using namespace Rcpp;

//' @noRd
// [[Rcpp::export]]
List find_dot_cpp(NumericVector close,
                  NumericVector join_date,
                  IntegerVector bias_id,
                  List targets) {

     // Extract target vectors
     NumericVector t_val = targets["dol_value"];
     StringVector t_lbl = targets["dol_label"];
     NumericVector t_wstart = targets["window_start"];
     NumericVector t_wend = targets["window_end"];
     IntegerVector t_fid = targets["formed_bias_id"];
     IntegerVector t_tid = targets["type_id"];

     int n_rows = close.size();
     int n_targets = t_val.size();

     IntegerVector out_id(n_rows, NA_INTEGER);
     NumericVector out_dot(n_rows, NA_REAL);
     CharacterVector out_label(n_rows, NA_STRING);

     // Loop through every historical data row
     for(int i = 0; i < n_rows; ++i) {
          double c_close = close[i];
          double c_date = join_date[i];
          int c_bid = bias_id[i];

          int best_id = NA_INTEGER;
          double best_val = NA_REAL;
          String best_lbl = NA_STRING;

          // Scan liquidity targets
          for(int j = 0; j < n_targets; ++j) {
               // 1. Check time window and ID boundaries
               if (t_wstart[j] <= c_date && t_wend[j] > c_date &&
                   t_fid[j] == c_bid) {

                    double target_v = t_val[j];

                    // Match condition logic (handled outside by passing
                    // filtered target tables)
                    // if (target_v != NA_REAL) {
                    if (!NumericVector::is_na(target_v)) {
                         // If we haven't found a target yet, or this one is
                         // closer to the close price
                         if (NumericVector::is_na(best_val) ||
                             std::abs(target_v - c_close) <
                                     std::abs(best_val - c_close)) {
                              best_val = target_v;
                              best_lbl = t_lbl[j];
                              best_id = t_tid[j];
                         }
                    }
               }
          }
          out_dot[i] = best_val;
          out_label[i] = best_lbl;
          out_id[i] = best_id;
     }

     return List::create(
             Named("dot") = out_dot,
             Named("dot_name") = out_label,
             Named("dot_id") = out_id
     );
}

