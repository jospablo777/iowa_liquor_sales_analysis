# We will use   an unsupervised anomaly detection algorithm: iForest

# Modified from Moez Ali tutorial
# https://pycaret.gitbook.io/docs/learn-pycaret/official-blog/time-series-anomaly-detection-with-pycaret

# Data load. We pull it from our R environment
time_series_data = r.data_iforest
time_series_data.dtypes

# Data prep
time_series_data.set_index('day', drop=True, inplace=True)


# PyCaret setup
from pycaret.anomaly import *
s = setup(time_series_data, session_id = 11)

# Train model
iforest = create_model('iforest', fraction = 0.1)
iforest_results = assign_model(iforest)
iforest_results.head()
