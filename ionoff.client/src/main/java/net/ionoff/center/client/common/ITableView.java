package net.ionoff.center.client.common;

import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.SimplePager;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.SingleSelectionModel;

import net.ionoff.center.client.zone.ToolBarView;

public interface ITableView<T> {
	Widget asWidget();
	
	CellTable<T> createCellTable();
	
	Column<T, String> getEditColumn();
	
	void showEditForm();

	ToolBarView getToolBarView();

	SimplePager getPager();

	CellTable<T> getCellTable();
	
	AsyncDataProvider<T> getDataProvider();

	SingleSelectionModel<T> getSingleSelectionModel();

	void setDataProvider(AsyncDataProvider<T> provider);

	public static final int PAGE_SIZE = 15;

	void hideEditForm();
}
