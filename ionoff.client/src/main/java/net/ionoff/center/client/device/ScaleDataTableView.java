package net.ionoff.center.client.device;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialDropDown;
import gwt.material.design.client.ui.MaterialLink;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDataDto;

public class ScaleDataTableView extends AbstractTableView<SensorDataDto> implements ScaleDataTablePresenter.Display {
	
	private Column<SensorDataDto, String> timeColumn;
	private Column<SensorDataDto, String> valueColumn;
	private Column<SensorDataDto, String> indexColumn;
	private Column<SensorDataDto, String> editColumn;
	private MaterialButton btnDownloadReport;

	private ScaleDataEditView scaleDataEditView;
	
	private MaterialDropDown dropDownReportFile;
	private MaterialLink linkDownloadPdfFile;
	private MaterialLink linkDownloadExcelFile;
	private MaterialLink linkDownloadWordFile;
	
	public ScaleDataTableView() {
		super("scaledata");
		getToolBarView().getLblTitle().setIconType(IconType.DEVICES_OTHER);
		getToolBarView().getBtnAdd().setVisible(false);
		getToolBarView().getBtnRemove().setVisible(false);
		getToolBarView().getTextBoxKeyWord().setVisible(false);
		getToolBarView().getPanelDateInput().setVisible(true);
		getToolBarView().setDayPeriod(7);
		
		btnDownloadReport = new MaterialButton();
		btnDownloadReport.setActivates("dropDownReportFile");
		btnDownloadReport.setIconType(IconType.FILE_DOWNLOAD);
		btnDownloadReport.setIconColor(Color.WHITE);
		getToolBarView().addBtn(btnDownloadReport);
		
		dropDownReportFile = new MaterialDropDown();
		dropDownReportFile.setActivator("dropDownReportFile");
		dropDownReportFile.setBelowOrigin(true);
		dropDownReportFile.setBackgroundColor(Color.GREY_LIGHTEN_5);
		dropDownReportFile.setConstrainWidth(false);
		dropDownReportFile.setWidth("220px");
		dropDownReportFile.setMarginTop(5);
		
		linkDownloadPdfFile = new MaterialLink("PDF (.pdf)");
		linkDownloadPdfFile.setIconType(IconType.FILE_DOWNLOAD);
		dropDownReportFile.add(linkDownloadPdfFile);
		
		linkDownloadExcelFile = new MaterialLink("EXEL (.xlsx)");
		linkDownloadExcelFile.setIconType(IconType.FILE_DOWNLOAD);
		dropDownReportFile.add(linkDownloadExcelFile);

		linkDownloadWordFile = new MaterialLink("WORD (.docx)");
		linkDownloadWordFile.setIconType(IconType.FILE_DOWNLOAD);
		dropDownReportFile.add(linkDownloadWordFile);
		
		getToolBarView().add(dropDownReportFile);
	}

	@Override
	public CellTable<SensorDataDto> createCellTable() {
		CellTable<SensorDataDto> cellTable = new CellTable<SensorDataDto>();
		
		TextColumn<SensorDataDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		timeColumn = createTimeColumn();
		cellTable.addColumn(timeColumn, AdminLocale.getAdminConst().date());
		cellTable.setColumnWidth(timeColumn, COLUMN_NAME_WIDTH, Unit.PX);

		valueColumn =  createValueColumn();
		cellTable.addColumn(valueColumn, AdminLocale.getAdminConst().total());
		cellTable.setColumnWidth(valueColumn, COLUMN_NAME_WIDTH, Unit.PX);

		indexColumn =  createIndexColumn();
		cellTable.addColumn(indexColumn, AdminLocale.getAdminConst().index());
		cellTable.setColumnWidth(indexColumn, 100, Unit.PX);

		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");

		return cellTable;
	}

	private Column<SensorDataDto, String> createTimeColumn() {
		Column<SensorDataDto, String> column = new TextColumn<SensorDataDto>() {
			@Override
			public String getValue(SensorDataDto object) {
				return object.getTime() == null ? "" : object.getTime().replaceAll(" 23:59:59", "");
			}
		};
		return column;
	}

	private Column<SensorDataDto, String> createValueColumn() {
	    Column<SensorDataDto, String> column = new TextColumn<SensorDataDto>() {
	    	@Override
	    	public String getValue(SensorDataDto object) {
				return String.valueOf(object.getValue());
	    	}
	    };
		return column;
	}

	private Column<SensorDataDto, String> createIndexColumn() {
		Column<SensorDataDto, String> column = new TextColumn<SensorDataDto>() {
			@Override
			public String getValue(SensorDataDto object) {
				return String.valueOf(object.getIndex());
			}
		};
		return column;
	}

	@Override
	public Column<SensorDataDto, String> getEditColumn() {
		return editColumn;
	}

	@Override
	public Column<SensorDataDto, String> getNameColumn() {
		return timeColumn;
	}
	
	@Override
	public MaterialLink getLinkDownloadPdfFile() {
		return linkDownloadPdfFile;
	}

	@Override
	public MaterialLink getLinkDownloadExcelFile() {
		return linkDownloadExcelFile;
	}

	@Override
	public MaterialLink getLinkDownloadWordFile() {
		return linkDownloadWordFile;
	}

	@Override
	public ScaleDataEditView getScaleDataEditView() {
		if (scaleDataEditView == null) {
			scaleDataEditView = new ScaleDataEditView();
			add(scaleDataEditView);
		}
		return scaleDataEditView;
	}

	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getScaleDataEditView().setStyleName("edit invisible");
	}

	@Override
	public void showEditForm() {
		super.showEditForm();
		getScaleDataEditView().setStyleName("edit visible");
	}
}
