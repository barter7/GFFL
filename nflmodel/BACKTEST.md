# Walk-forward backtest record

Generated 2026-07-15 with the constants in model_core.R.
2021-2024 calibrate the constants; 2024 tuned QB_TDR_PRIOR_GAMES,
ANY_TD_RESID_COMPRESS and the empirical TD shape; 2025 is untouched holdout.

## 2025 (holdout)
```
================ BACKTEST SUMMARY ================

[Receptions]  n = 3144
  MAE  model 1.323 | naive 1.339
  Calibration of P(over synthetic line):
     bucket    n  pred   emp
1 (0.2,0.3]  198 0.274 0.308
2 (0.3,0.4] 1171 0.361 0.379
3 (0.4,0.5] 1558 0.447 0.468
4 (0.5,0.6]  217 0.519 0.544

[QB pass yards]  n = 393
  MAE  model 61.2 | naive 65.4
  PIT deciles (should each be ~0.10):

  (0,0.1] (0.1,0.2] (0.2,0.3] (0.3,0.4] (0.4,0.5] (0.5,0.6] (0.6,0.7] (0.7,0.8] 
    0.112     0.125     0.092     0.076     0.099     0.125     0.120     0.081 
(0.8,0.9]   (0.9,1] 
    0.069     0.102 

[QB pass TDs]  n = 393
  MAE 0.982 | mean exp 1.489 vs mean actual 1.397
  P(2+ TD): predicted 0.445 | empirical 0.425

[Anytime TD]  n = 3558
  Brier  model 0.1541 | naive 0.1646 | base rate 0.216
  Calibration:
     bucket    n  pred    emp
1   (0,0.1]  723 0.068 0.0913
2 (0.1,0.2] 1264 0.146 0.1361
3 (0.2,0.3]  765 0.246 0.2797
4 (0.3,0.4]  473 0.345 0.3531
5 (0.4,0.5]  211 0.447 0.3791
6 (0.5,0.7]  120 0.561 0.5500
7   (0.7,1]    2 0.706 1.0000
```

## 2024 (tuning year)
```
================ BACKTEST SUMMARY ================

[Receptions]  n = 2829
  MAE  model 1.358 | naive 1.374
  Calibration of P(over synthetic line):
     bucket    n  pred   emp
1 (0.2,0.3]  201 0.273 0.284
2 (0.3,0.4] 1026 0.358 0.375
3 (0.4,0.5] 1397 0.448 0.489
4 (0.5,0.6]  205 0.518 0.571

[QB pass yards]  n = 373
  MAE  model 66.7 | naive 67.8
  PIT deciles (should each be ~0.10):

  (0,0.1] (0.1,0.2] (0.2,0.3] (0.3,0.4] (0.4,0.5] (0.5,0.6] (0.6,0.7] (0.7,0.8] 
    0.105     0.091     0.091     0.102     0.107     0.075     0.107     0.097 
(0.8,0.9]   (0.9,1] 
    0.110     0.115 

[QB pass TDs]  n = 373
  MAE 0.954 | mean exp 1.414 vs mean actual 1.418
  P(2+ TD): predicted 0.420 | empirical 0.448

[Anytime TD]  n = 3234
  Brier  model 0.1593 | naive 0.1682 | base rate 0.224
  Calibration:
     bucket    n  pred    emp
1   (0,0.1]  611 0.071 0.0933
2 (0.1,0.2] 1194 0.146 0.1474
3 (0.2,0.3]  702 0.245 0.2906
4 (0.3,0.4]  411 0.344 0.3309
5 (0.4,0.5]  205 0.447 0.4390
6 (0.5,0.7]  111 0.556 0.5586
```
