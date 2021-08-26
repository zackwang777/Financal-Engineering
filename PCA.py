import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

def getPCA(PCA_df):
    # Clean data
    features = ['DV01', 'Duration', 'Modified Duration', 'Convexity', 'Yield to Worst', 'Bid Ask Spread']
    PCA_info = ['Description', 'TRBC Economic Sector Name', 'Quantity']
    PCA_info.extend(features)
    PCA_df = PCA_df[PCA_info].dropna().reset_index(drop=True)

    if not PCA_df.empty:
        # PCA
        feature = PCA_df[features]
        x = StandardScaler().fit_transform(feature)
        pca = PCA(n_components=2)
        principalComponents = pca.fit_transform(x)
        principalDf = pd.DataFrame(data=principalComponents
                                   , columns=['principal component 1', 'principal component 2'])

        # Put all data in one DataFrame.
        finalDf = pd.concat([PCA_df, principalDf], axis=1)

        # Draw figure.
        fig = px.scatter(finalDf, x="principal component 1", y="principal component 2", size="Quantity",
                         color="TRBC Economic Sector Name", hover_name="Description",
                         hover_data={'Quantity': True, 'DV01': True, 'Duration': True, 'Modified Duration': True,
                                     'Convexity': True, 'Yield to Worst': True, 'Bid Ask Spread': True,
                                     'principal component 1': False, 'principal component 2': False},
                         template='seaborn')
        fig.update_layout(hoverlabel_align='right', title="Set hover text with hovertemplate")
        fig.update_layout(title="Principal Component Analysis", xaxis_title="", yaxis_title="")
    else:
        fig = go.Figure()
    return fig

# Get data of fixed income assets from the parquet file.
PCA_df = pd.read_parquet(path=folder + '\\portfolio.parquet.gzip')
PCA_df = PCA_df.loc[PCA_df['Asset_Class'] == 'Fixed Income']
PCA_fig = getPCA(PCA_df)

