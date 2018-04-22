package net.ionoff.center.client.user;

import com.google.gwt.cell.client.EditTextCell;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.UserDto;

public class UserTableView extends AbstractTableView<UserDto> implements SystemUsersPresenter.Display {
	
	private Column<UserDto, String> nameColumn;
	private Column<UserDto, String> fullNameColumn;
	private Column<UserDto, String> editColumn;
	
	private UserEditView userEditView;
	
	public UserTableView() {
		super("users");
		
		getToolBarView().getLblTitle().setIconType(IconType.ACCOUNT_CIRCLE);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().user());
	}

	@Override
	public CellTable<UserDto> createCellTable() {
		CellTable<UserDto> cellTable = new CellTable<UserDto>();
		
		TextColumn<UserDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		fullNameColumn = createFullNameColumn();
		cellTable.addColumn(fullNameColumn, AdminLocale.getAdminConst().fullName());
		cellTable.setColumnWidth(fullNameColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}
	
	@Override
	public Column<UserDto, String> getNameColumn() {
		return nameColumn;
	}
	
	protected Column<UserDto, String> createFullNameColumn() {
		Column<UserDto, String> column =  new Column<UserDto, String>(new EditTextCell()) { 
			@Override
			public String getValue(UserDto object) {
				return object.getFullName() != null ? object.getFullName() : "";
			}
		};
		return column;
	}
	@Override
	public Column<UserDto, String> getFullNameColumn() { 
		return this.fullNameColumn; 
	}
	
	
	@Override
	public Column<UserDto, String> getEditColumn() {
		return editColumn;
	}
	

	@Override
	public UserEditView getUserEditView() {
		if (userEditView == null) {
			userEditView = new UserEditView();
			add(userEditView);
		}
		return userEditView;
	}
	
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getUserEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getUserEditView().setStyleName("edit visible");
	} 
}
