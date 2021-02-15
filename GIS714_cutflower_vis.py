import dash
from dash.dependencies import Input, Output, State
import dash_html_components as html
import dash_core_components as dcc

import pandas as pd
import plotly.graph_objs as go

app = dash.Dash(__name__)
app.title = 'US Cut Flower Imports'
app.css.append_css({'external_url': 'https://codepen.io/amyoshino/pen/jzXypZ.css'})

dfReg = pd.read_csv('dash_flowerRegion.csv')
dfPOE = pd.read_csv('dash_flowerPOE.csv')
dfMon = pd.read_csv('dash_flowerMonth.csv')
dfRegGeo = pd.read_csv('dash_flowerOriginGeo.csv')
dfPOEGeo = pd.read_csv('dash_flowerPOEGeo.csv')

countries = dfRegGeo.Code.unique()
country_names = dfRegGeo.ORIGIN_NM.unique()
states = dfPOEGeo.State.unique()

app.layout = html.Div(children=[
    html.H2(
        children='2018 US Cut Flower Imports',
        style={
            'textAlign': 'center'
        }
    ),

    html.Div(
        children='''Select one or more flower types to explore cut flower import pathways.''',
         style={
            'textAlign': 'left',
            'padding-bottom':10
        }
    ),
        dcc.Dropdown(
        id='order-select',
        options=[{'label': i, 'value': i} for i in sorted(dfPOE.Order.unique())],
        value=[],
        multi=True,
        placeholder="Select a flower taxonomic order",
        style={
                         "margin-left": "auto",
                         "margin-right": "auto",
                         "width": "100%"

                     }
    ),
    dcc.Checklist(id='select-all',
                  options=[{'label': 'Select All', 'value': 1}], 
                  values=[]),
    html.Div([
        html.H4(
        children='''US Ports of Entry - grouped by state''',
         style={
            'textAlign': 'center'
        }
        ),
        html.Div([
            dcc.Graph(
                id='map-poe'
            ), 
            ], className = 'seven columns'
            ),
            html.Div([
            dcc.Graph(
                id='bar-poe',
            ),
            ], className = 'five columns'
            ),
    ], className ="row", style={'padding': 5}
    ),
    html.Div([
        html.H4(
        children='''Country or Region of Origin''',
         style={
            'textAlign': 'center'
        }
        ),
        html.Div([
            dcc.Graph(
                id='map-reg'
            ), 
            ], className = 'seven columns'
            ),
        html.Div([
            dcc.Graph(
                id='bar-reg'
            ), 
            ], className = 'five columns'
            ),
    ], className ="row", style={'padding': 5}
    ),
    html.Div([
    html.H4(
        children='Import Seasonality',
         style={
            'textAlign': 'center'
        }
        ),
    dcc.Graph(
        id='timeline'
    ),
     html.Div(
                [
                    html.P('Kellyn Montgomery - GIS 715 Geovisualization Product, Spring 2018', style = {'display': 'inline'})
                ], className = "twelve columns",
                    style = {'fontSize': 14, 'padding-top': 20}
            ),
            html.Div(
                [
                    html.P('Data source: USDA APHIS AQAS', style = {'display': 'inline'})
                ], className = "twelve columns",
                    style = {'fontSize': 14}
            )
        ], className ="row"
    ),
], className='ten columns offset-by-one')


@app.callback(
    Output('order-select', 'value'),
    [Input('select-all', 'values')],
    [State('order-select', 'options'),
     State('order-select', 'value')])
def test(selected, options, values):
    if selected[0] == 1:
        return [i['value'] for i in options]
    else:
        return values


@app.callback(
    Output('map-poe', 'figure'), 
    [Input('order-select', 'value')])

def update_figure(selected_orders):
    new_dfPOEGeo = dfPOEGeo[dfPOEGeo['Order'].isin(selected_orders)]
    filtered_data = [go.Choropleth(
                        colorscale = [
                            [0.0, '#e8e8e8'],
                            [0.01, '#deebf7'],
                            [0.05, "#c6dbef"],
                            [0.1, "#9ecae1"],
                            [0.2, "#6baed6"],
                            [0.3, "#4292c6"],
                            [0.5, "#2171b5"],
                            [1, "#084594"],
                        ],
                        locations = sorted(new_dfPOEGeo.State.unique()),
                        z = new_dfPOEGeo.groupby('State')['N'].sum(),
                        locationmode = 'USA-states',
                        text = ["State: {} <br>Num of shipments: {:,} <br>Num of stems: {:,}".format(i,j,k,) 
                            for i,j,k in zip(sorted(new_dfPOEGeo.State.unique()), new_dfPOEGeo.groupby('State')['N'].sum(), 
                            new_dfPOEGeo.groupby('State')['total'].sum())],
                        hoverinfo='text',
                        marker = go.choropleth.Marker(
                            line = go.choropleth.marker.Line(
                                color = 'rgb(0,0,0)',
                                width = 1
                            )),
                        colorbar = go.choropleth.ColorBar(
                            title = "Num of<br>Shipments",
                            x=0,
                            y=0.4,
                            len=0.4,
                            thickness=15)
                    )]

    return {
        'data': filtered_data,
        'layout': go.Layout(
                    geo = go.layout.Geo(
                            scope = 'usa',
                            showframe = False,
                            showcoastlines = False,
                            projection = go.layout.geo.Projection(type = 'albers usa'),
                            showlakes = False),
                    margin=dict(
                        l=20,
                        r=5,
                        b=10,
                        t=5
                    ),
                )
    }



@app.callback(
    Output('bar-poe', 'figure'),
    [Input('order-select', 'value')])

def update_figure(selected_orders):
    new_dfPOE = dfPOE[dfPOE['Order'].isin(selected_orders)]
    filtered_data = [
        go.Bar(
                    x=new_dfPOE[new_dfPOE['Outcome']==i]['State'],
                    y=new_dfPOE[new_dfPOE['Outcome']==i]['N'],
                    text=["Order: {} <br>Outcome: {} <br>Num of shipments: {:,} <br>Num of stems: {:,}".format(i,j,k,l) 
                        for i,j,k,l in zip(new_dfPOE[new_dfPOE['Outcome'] == i]['Order'], new_dfPOE[new_dfPOE['Outcome'] == i]['Outcome'], 
                        new_dfPOE[new_dfPOE['Outcome'] == i]['N'], new_dfPOE[new_dfPOE['Outcome'] == i]['total'])],
                    hoverinfo='text',
                    name=i
                ) for i in new_dfPOE.Outcome.unique() 
               ]

    return {
        'data': filtered_data,
        'layout': go.Layout(
                barmode='stack',
                colorway=["#2171b5","#d7191c", "#efe339", "#6baed6", "#f78e0e"],
                yaxis={'title': 'Num of Shipments'},
                separators=",",
                hovermode='closest',
                legend=dict(x=0.7, 
                y=0.95),
                margin=dict(
                        l=60,
                        r=15,
                        b=80,
                        t=30
                    ),
                annotations = [go.layout.Annotation(
                    x = 1.07,
                    y = -0.2,
                    xref = 'paper',
                    yref = 'paper',
                    text = '<i>States with at least 1,000<br>imported shipments in 2018</i>',
                    showarrow = False,
                    font=dict(size=10),
                    align='right'
                )]
            )
    }

@app.callback(
    Output('map-reg', 'figure'), 
    [Input('order-select', 'value')])

def update_figure(selected_orders):
    new_dfRegGeo = dfRegGeo[dfRegGeo['Order'].isin(selected_orders)]
    filtered_data = [go.Choropleth(
                        colorscale = [
                            [0, '#ffffff'],
                            [0.001, '#e8e8e8'],
                            [0.01, '#deebf7'],
                            [0.05, "#c6dbef"],
                            [0.1, "#9ecae1"],
                            [0.2, "#6baed6"],
                            [0.3, "#4292c6"],
                            [0.5, "#2171b5"],
                            [1, "#084594"],
                        ],
                        locations = sorted(new_dfRegGeo.ORIGIN_NM.unique()),
                        locationmode="country names",
                        z = new_dfRegGeo.groupby('ORIGIN_NM')['N'].sum(),
                        text = ["Country: {} <br>Num of shipments: {:,} <br>Num of stems: {:,}".format(i,j,k,) 
                            for i,j,k in zip(sorted(new_dfRegGeo.ORIGIN_NM.unique()), new_dfRegGeo.groupby('ORIGIN_NM')['N'].sum(), new_dfRegGeo.groupby('ORIGIN_NM')['total'].sum())],
                        hoverinfo='text',
                        marker = go.choropleth.Marker(
                            line = go.choropleth.marker.Line(
                                color = 'rgb(0,0,0)',
                                width = 1
                            )),
                        colorbar = go.choropleth.ColorBar(
                            title = "Num of<br>Shipments<br> ",
                            len=0.4,
                            x=0,
                            y=0.4,
                            thickness=15)
                    )]

    return {
        'data': filtered_data,
        'layout': go.Layout(
                    geo = go.layout.Geo(
                        showframe = False,
                        scope='world',
                        showcountries=True,
                        showcoastlines = False,
                        projection = go.layout.geo.Projection(type = 'equirectangular'),
                        countrycolor='rgb(0,0,0)',
                        countrywidth=1
                    ),
                    margin=dict(
                        l=5,
                        r=25,
                        b=0,
                        t=5
                    )
                )
    }



@app.callback(
    Output('bar-reg', 'figure'),
    [Input('order-select', 'value')])

def update_figure(selected_orders):
    new_dfReg = dfReg[dfReg['Order'].isin(selected_orders)]
    filtered_data = [
        go.Bar(
            x=new_dfReg[new_dfReg['Outcome']==i]['Subregion'],
            y=new_dfReg[new_dfReg['Outcome']==i]['N'],
            text=["Order: {} <br>Outcome: {} <br>Num of shipments: {:,} <br>Num of stems: {:,}".format(i,j,k,l) 
                for i,j,k,l in zip(new_dfReg[new_dfReg['Outcome'] == i]['Order'], new_dfReg[new_dfReg['Outcome'] == i]['Outcome'], 
                new_dfReg[new_dfReg['Outcome'] == i]['N'], new_dfReg[new_dfReg['Outcome'] == i]['total'])],
            hoverinfo='text',
            name=i
        ) for i in new_dfReg.Outcome.unique()
        ]

    return {
        'data': filtered_data,
        'layout': go.Layout(
                barmode='stack',
                colorway=["#2171b5","#d7191c", "#efe339", "#6baed6", "#f78e0e"],
                yaxis={'title': 'Num of Shipments'},
                hovermode='closest',
                legend=dict(x=0.7, 
                y=0.95),
                margin=dict(
                        l=60,
                        r=15,
                        b=93,
                        t=30
                    ),
                annotations = [go.layout.Annotation(
                    x = 1.07,
                    y = -0.29,
                    xref = 'paper',
                    yref = 'paper',
                    text = '<i>Regions with at least 1,000<br>exported shipments in 2018</i>',
                    showarrow = False,
                    font=dict(size=10),
                    align='right'
                )]
            )
    }


@app.callback(
    Output('timeline', 'figure'),
    [Input('order-select', 'value')])

def update_figure(selected_orders): 
    new_dfMon = dfMon[dfMon['Order'].isin(selected_orders)]
    filtered_data = [
        go.Scatter(
                    x=new_dfMon[new_dfMon['Order'] == i]['Month'],
                    y=new_dfMon[new_dfMon['Order'] == i]['N'],
                    text=["Order: {} <br>Month: {} <br>Num of shipments: {:,} <br>Num of stems: {:,} <br>".format(i,j,k,l) for i,j,k,l in zip(new_dfMon[new_dfMon['Order'] == i]['Order'], 
                        new_dfMon[new_dfMon['Order'] == i]['Month'], new_dfMon[new_dfMon['Order'] == i]['N'], new_dfMon[new_dfMon['Order'] == i]['total'])],
                    hoverinfo='text',
                    mode='lines+markers',
                        name=i
                    ) for i in new_dfMon.Order.unique()
               ]

    return {
        'data': filtered_data,
        'layout': go.Layout(
                yaxis={'title': 'Num of Shipments'},
                margin=dict(t=30),
                hovermode='closest',
            )
    }


# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)