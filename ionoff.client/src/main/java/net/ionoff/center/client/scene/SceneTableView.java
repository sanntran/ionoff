package net.ionoff.center.client.scene;

import com.google.gwt.cell.client.ClickableTextCell;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.SceneDto;

public class SceneTableView extends AbstractTableView<SceneDto> implements SceneTablePresenter.Display {
	
	private Column<SceneDto, String> nameColumn;
	private Column<SceneDto, String> zoneColumn;
	private Column<SceneDto, String> orderColumn;
	private Column<SceneDto, String> editColumn;
	private SceneEditView sceneEditView;
	
	public SceneTableView() {
		super("scenes");
		getToolBarView().getLblTitle().setIconType(IconType.MOVIE);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().scene());
	}

	@Override
	public CellTable<SceneDto> createCellTable() {
		CellTable<SceneDto> cellTable = new CellTable<SceneDto>();
		
		TextColumn<SceneDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);

		zoneColumn = createZoneColumn();
		zoneColumn.setSortable(true);
		cellTable.addColumn(zoneColumn, AdminLocale.getAdminConst().zone());
		cellTable.setColumnWidth(zoneColumn, 150.0, Unit.PX);
		
		orderColumn = createOrderColumn();
		orderColumn.setSortable(true);
		cellTable.addColumn(orderColumn, AdminLocale.getAdminConst().order());
		cellTable.setColumnWidth(orderColumn, 70, Unit.PX);

		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		
		return cellTable;
	}
	
	@Override
	public Column<SceneDto, String> getNameColumn() {
		return nameColumn;
	}
	
	protected Column<SceneDto, String> createZoneColumn() {
	    Column<SceneDto, String> column = new Column<SceneDto, String>(new ClickableTextCell()) {
	    	@Override
	    	public String getValue(SceneDto object) {
	    		if (object.getZoneId() == null || object.getZoneName() == null) {
	    			return "";
	    		}
	    		return BaseDto.formatNameID(object.getZoneName(), object.getZoneId());
	    	}
	    };
		return column;
	}

	private Column<SceneDto, String> createOrderColumn() {
		Column<SceneDto, String> column = new TextColumn<SceneDto>() {
			@Override
			public String getValue(SceneDto object) {
			return object.getOrder() == null ? "" : object.getOrder().toString();
			}
		};
		return column;
	}
	@Override
	public Column<SceneDto, String> getAreaColumn() {
		return zoneColumn;
	}

	@Override
	public Column<SceneDto, String> getEditColumn() {
		return editColumn;
	}
	
	@Override
	public SceneEditView getSceneEditView() {
		if (sceneEditView == null) {
			sceneEditView = new SceneEditView();
			add(sceneEditView);
		}
		return sceneEditView;
	}
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getSceneEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getSceneEditView().setStyleName("edit visible");
	}
}
