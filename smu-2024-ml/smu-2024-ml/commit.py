'''
这是最好一次提交的背后的代码
(可能把,因为我有好多个版本,不太确定,不过也无所谓了)
由于只要考虑输出.csv就行,所以降维,生成三维网格之类的代码就统统不要了
'''
import os
import random
import pandas as pd
import numpy as np
from sklearn.model_selection import StratifiedKFold, RandomizedSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import log_loss
from sklearn.ensemble import RandomForestClassifier
from tqdm.auto import tqdm

np.random.seed(42)
random.seed(42)
os.environ['PYTHONHASHSEED'] = str(42)

df_train = pd.read_csv("input/train.csv")
df_test = pd.read_csv("input/test.csv")

# 参数
class configs:
    target_col = "target"
    unused_col = [target_col, "fold", "id"]
    FOLDS = 5

configs.feat_cols = [col for col in df_train.columns if col not in configs.unused_col]
y_train = df_train[configs.target_col]
X_train = df_train[configs.feat_cols]
X_test = df_test[configs.feat_cols]

scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

'''
调整权重
基本思路就是调整这24个维度数据的权重,之后再超参数优化一下,异常值这个没去管它
至于具体权重是怎么算出来的?一部分看得懂的先填一下大致关系,看不懂的不管了
之后再根据loss数据反推
有点根据答案出题目的意思,哈哈😄
'''
feature_weights = {
    'risk_assessment_score': 3.0,  # 高风险评估得分与违约风险直接相关
    'first_trade_elapsed_time': 1.5,  # 客户交易历史越长,违约风险越低
    'recent_trade_gap': 1.2,  # 近期交易活跃度越高,违约风险越低
    'avg_time_to_resolve': 1.0,  # 平均问题解决时间,不知道
    'total_successful_trades': 2.5,  # 成功交易总数越多,违约风险越低
    'delinquent_trades_60_days': 3.0,  # 60天以上拖欠交易越多,违约风险越高
    'delinquent_trades_90_days': 3.0,  # 90天以上拖欠交易越多,违约风险越高
    'legal_trade_percentage': 1.0,  # 合法交易百分比
    'last_illegal_trade_gap': 2.5,  # 最近一次非法交易时间
    'illegal_trades_last_year_max': 3.0,  # 过去一年内最大非法交易
    'total_illegal_trades_max': 3.0,  # 所有非法交易中的最大值
    'total_trade_count': 2.0,  # 总交易数越多,违约风险越低
    'recently_initiated_trades': 1.5,  # 最近发起的交易数
    'installment_trade_percentage': 1.0,  # 分期付款交易百分比
    'non_recent_inquiry_gap': 1.0,  # 较长时间内的信用查询
    'recent_inquiries_count': 2.0,  # 近期内的查询总数
    'non_recent_inquiries_count': 1.0,  # 非近期的查询总数
    'revolving_debt_ratio': 2.5,  # 循环信用负债比例
    'installment_debt_ratio': 2.0,  # 分期付款负债比例
    'revolving_trades_with_balance': 2.0,  # 有余额的循环信用交易数
    'installment_trades_with_balance': 2.0,  # 有余额的分期付款交易数
    'high_risk_banks_count': 3.0,  # 高风险账户数
    'trades_with_balance_percentage': 2.5  # 有余额的交易百分比
}

# 根据权重调整训练和测试数据
X_train_weighted = X_train_scaled.copy()
X_test_weighted = X_test_scaled.copy()

for feature, weight in feature_weights.items():
    X_train_weighted[:, configs.feat_cols.index(feature)] *= weight
    X_test_weighted[:, configs.feat_cols.index(feature)] *= weight

# 随机森林
rf_model = RandomForestClassifier(random_state=42)
'''
超参数调优的参数
这段怎么来的?不会写,让gpt帮我写的🤣
这玩意真的恐怖,前面的代码吭哧吭哧写半天,这几行按个回车一下子就搞好了
'''
param_dist = {
    'n_estimators': [100, 200, 500],
    'max_depth': [3, 6, 10, None],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4],
    'bootstrap': [True, False]
}
# 超参数调优
random_search = RandomizedSearchCV(estimator=rf_model, param_distributions=param_dist, n_iter=100, scoring='neg_log_loss', cv=StratifiedKFold(n_splits=configs.FOLDS), verbose=1, random_state=42, n_jobs=-1)
random_search.fit(X_train_weighted, y_train)
best_rf_model = random_search.best_estimator_

# 交叉验证和预测
# 和原先svm-baseline的代码差不多
cv = StratifiedKFold(n_splits=configs.FOLDS)
y_pred_train = np.zeros(len(X_train))
y_pred_test = np.zeros(len(X_test))
scores = []

for train_index, val_index in tqdm(cv.split(X_train_weighted, y_train)):
    X_train_, y_train_ = X_train_weighted[train_index], y_train[train_index]
    X_val, y_val = X_train_weighted[val_index], y_train[val_index]
    best_rf_model.fit(X_train_, y_train_)
    y_pred_val = best_rf_model.predict_proba(X_val)[:, 1]
    y_pred_train[val_index] = y_pred_val
    y_pred_test += best_rf_model.predict_proba(X_test_weighted)[:, 1] / configs.FOLDS
    score = log_loss(y_val, y_pred_val)
    scores.append(score)
    print(f"Fold Log Loss: {score:.6f}")

print(f"Overall Log Loss: {np.mean(scores):.6f} ± {np.std(scores):.6f}")

submission = pd.DataFrame({
    'id': df_test['id'],
    'target': y_pred_test
})
submission.to_csv('output/submission-5.csv', index=False)
