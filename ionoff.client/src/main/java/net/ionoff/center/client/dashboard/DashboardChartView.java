package net.ionoff.center.client.dashboard;

import org.moxieapps.gwt.highcharts.client.AxisTitle;
import org.moxieapps.gwt.highcharts.client.Chart;
import org.moxieapps.gwt.highcharts.client.Credits;
import org.moxieapps.gwt.highcharts.client.Legend;
import org.moxieapps.gwt.highcharts.client.Series;
import org.moxieapps.gwt.highcharts.client.Style;
import org.moxieapps.gwt.highcharts.client.ToolTip;
import org.moxieapps.gwt.highcharts.client.ToolTipData;
import org.moxieapps.gwt.highcharts.client.ToolTipFormatter;
import org.moxieapps.gwt.highcharts.client.labels.DataLabels;
import org.moxieapps.gwt.highcharts.client.labels.XAxisLabels;
import org.moxieapps.gwt.highcharts.client.plotOptions.BarPlotOptions;

import com.google.gwt.user.client.ui.FlowPanel;


public class DashboardChartView extends FlowPanel {

	private final Chart chart;
	private final Series series;
	
	
	public DashboardChartView(String itemStyle, String itemName) {
		
		chart = new Chart()  
	            .setType(Series.Type.BAR)  
	            .setChartTitleText("") 
	            .setBarPlotOptions(new BarPlotOptions()  
	                .setDataLabels(new DataLabels()  
	                    .setEnabled(true)  
	                )  
	            )  
	            .setLegend(new Legend()
	                .setEnabled(false)
	            )  
	            .setCredits(new Credits()  
	                .setEnabled(false)  
	            )  
	            .setToolTip(new ToolTip()  
	                .setFormatter(new ToolTipFormatter() {  
	                    @Override
						public String format(ToolTipData toolTipData) {  
	                        return toolTipData.getSeriesName() + ": " + toolTipData.getYAsLong() +" %";  
	                    }  
	                })  
	            );  
	  
	        chart.getXAxis()  
	            .setCategories("RAM", "Disk")
	            .setLineColor("transparent")
	            .setLabels(new XAxisLabels().setStyle(new Style().setColor("#a7ffeb").setFontSize("16px")));
	  
	        chart.getYAxis()  
	            .setAxisTitle(new AxisTitle()  
	                .setText("")  
	                .setAlign(AxisTitle.Align.HIGH)
	            )
	            .setGridLineColor("transparent");  
	  
	        series = chart.createSeries()  
            .setName("Used")  
            .setPoints(new Number[] {0, 0});
	        
	        chart.addSeries(series);   
			
		chart.setBackgroundColor("#455a64");
		
        chart.setHeight(118);
        chart.setWidth(150);
        chart.setMargin(0, 0, 0, 65);
        chart.setStyle(new Style().setOption("float", "left"));
        add(chart);        
	}
	
	public void setValue(int ramUsed, int diskUsed) {
		series.getPoints()[0].update(ramUsed);
		series.getPoints()[1].update(diskUsed);
        chart.redraw();        
	}
}
