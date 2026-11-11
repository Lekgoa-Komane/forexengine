#include <Rcpp.h>
using namespace Rcpp;

//' @noRd
// [[Rcpp::export]]
List process_mitigation_cpp(List registry, List dt) {

     // Extract registry columns
     NumericVector r_formed = registry["formed_at"];
     CharacterVector r_type = registry["type"];
     NumericVector r_crit = registry["critical_value"];
     NumericVector r_zbot = registry["type_zone_bot_value"];
     NumericVector r_ztop = registry["type_zone_top_value"];
     CharacterVector r_irule = registry["invalid_rule"];
     CharacterVector r_mrule = registry["mitig_rule"];
     CharacterVector r_op = registry["rel_op"];

     int n_reg = r_formed.size();

     // Extract main data table columns
     NumericVector d_date = dt["join_date"];
     NumericVector d_high = dt["high"];
     NumericVector d_low = dt["low"];
     NumericVector d_close = dt["close"];

     int n_dt = d_date.size();

     // Initialize output vectors for registry
     NumericVector out_invalidated(n_reg, NA_REAL);
     NumericVector out_mit_swing(n_reg, NA_REAL);
     NumericVector out_mit_prox(n_reg, NA_REAL);
     NumericVector out_mit_dist(n_reg, NA_REAL);

     // Loop through every entry in the registry
     for (int i = 0; i < n_reg; ++i) {
          double f_at = r_formed[i];
          String type = r_type[i];
          double crit = r_crit[i];
          double zbot = r_zbot[i];
          double ztop = r_ztop[i];
          String irule = r_irule[i];
          String mrule = r_mrule[i];
          String op = r_op[i];

          // Flags to stop looking once a timestamp is found (mult = "first")
          bool found_inv = false;
          bool found_swing = false;
          bool found_prox = false;
          bool found_dist = false;

          // Linear scan through the OHLC data table chronologically
          for (int j = 0; j < n_dt; ++j) {
               double c_date = d_date[j];

               // We only care about price data AFTER the level was formed
               if (c_date > f_at) {

                    // 1. SECTION 1: INVALIDATION LOOKUP
                    if (!found_inv && irule == "close") {
                         if (op == " > " && d_close[j] > crit) {
                              out_invalidated[i] = c_date;
                              found_inv = true;
                         } else if (op == " < " && d_close[j] < crit) {
                              out_invalidated[i] = c_date;
                              found_inv = true;
                         }
                    }

                    // 2. SECTION 2: MITIGATION LOOKUP
                    // Bearish Cases (Look for highs breaking thresholds)
                    if (op == " > ") {
                         if (!found_swing && mrule == "high" && type == "swing_high" && d_high[j] > crit) {
                              out_mit_swing[i] = c_date;
                              found_swing = true;
                         }
                         if (type == "bear_fvg" || type == "bear_ob") {
                              if (!found_prox && mrule == "high" && d_high[j] > zbot) {
                                   out_mit_prox[i] = c_date;
                                   found_prox = true;
                              }
                              if (!found_dist && mrule == "high" && d_high[j] > ztop) {
                                   out_mit_dist[i] = c_date;
                                   found_dist = true;
                              }
                         }
                    }
                    // Bullish Cases (Look for lows breaking thresholds)
                    else if (op == " < ") {
                         if (!found_swing && mrule == "low" && type == "swing_low" && d_low[j] < crit) {
                              out_mit_swing[i] = c_date;
                              found_swing = true;
                         }
                         if (type == "bull_fvg" || type == "bull_ob") {
                              if (!found_prox && mrule == "low" && d_low[j] < ztop) {
                                   out_mit_prox[i] = c_date;
                                   found_prox = true;
                              }
                              if (!found_dist && mrule == "low" && d_low[j] < zbot) {
                                   out_mit_dist[i] = c_date;
                                   found_dist = true;
                              }
                         }
                    }

                    // Short-circuit: optimization to break early if all possible values for this row are found
                    if (found_inv && (found_swing || found_prox || found_dist)) {
                         break;
                    }
               }
          }
     }

     return List::create(
          Named("invalidated_at")   = out_invalidated,
          Named("mit_swing_at")     = out_mit_swing,
          Named("mit_zone_prox_at") = out_mit_prox,
          Named("mit_zone_dist_at") = out_mit_dist
     );
}
