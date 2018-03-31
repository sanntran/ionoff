package net.ionoff.center.client.project;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.common.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.ProjectDto;

public class ProjectTableView extends AbstractTableView<ProjectDto> implements ProjectTablePresenter.Display {
	
	private Column<ProjectDto, String> nameColumn;
	private Column<ProjectDto, String> addressColumn;
	private Column<ProjectDto, String> editColumn;
	ProjectEditView projectEditView;
	
	public ProjectTableView() {
		super("projects");
		getToolBarView().getLblTitle().setIconType(IconType.BUSINESS);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().project());
	}

	@Override
	public CellTable<ProjectDto> createCellTable() {
		CellTable<ProjectDto> cellTable = new CellTable<ProjectDto>();
		
		TextColumn<ProjectDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);

		addressColumn =  createAddressColumn();
		cellTable.addColumn(addressColumn, AdminLocale.getAdminConst().address());
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}

	private Column<ProjectDto, String> createAddressColumn() {
		Column<ProjectDto, String> column = new TextColumn<ProjectDto>() { 
			@Override
			public String getValue(ProjectDto object) {
				if (object == null) {
					return "";
				}
				return object.getAddress();
			}
		};
		return column;
	}

	@Override
	public Column<ProjectDto, String> getNameColumn() {
		return nameColumn;
	}
	@Override
	public Column<ProjectDto, String> getAddressColumn() {
		return addressColumn;
	}

	@Override
	public Column<ProjectDto, String> getEditColumn() {
		return editColumn;
	}
	
	@Override
	public ProjectEditView getProjectEditView() {
		if (projectEditView == null) {
			projectEditView = new ProjectEditView();
			add(projectEditView);
		}
		return projectEditView;
	}
	
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getProjectEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getProjectEditView().setStyleName("edit visible");
	}
}
