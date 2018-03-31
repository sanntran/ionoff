package net.ionoff.center.client.common;

import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.SimplePager;
import com.google.gwt.user.cellview.client.TextColumn;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.SingleSelectionModel;

import net.ionoff.center.client.zone.ToolBarView;
import net.ionoff.center.shared.dto.BaseDto;

public abstract class AbstractTableView<T extends BaseDto> extends FlowPanel implements ITableView<T> {
	protected static final String ID = "ID";
	protected static final double COLUMN_ID_WIDTH = 30.0;
	protected static final double COLUMN_NAME_WIDTH = 150.0;
	
	private ToolBarView toolBarView;
	private CellTable<T> cellTable;
	private SimplePager simplePager;
	private SingleSelectionModel<T> singleSelectionModel;
	private AsyncDataProvider<T> dataProvider;
	private FlowPanel listPanel;
	
	public AbstractTableView(String style) {
		
		setStyleName(style + " table");
		
		listPanel = new FlowPanel();
		listPanel.setStyleName("list expanded");
		add(listPanel);
		
		// /////////////////////////////////////////////////
		toolBarView = new ToolBarView();
		listPanel.add(toolBarView);

		SimplePanel cellTableContainer = new SimplePanel();
		cellTableContainer.setStyleName("celltable");
		listPanel.add(cellTableContainer);
		
		cellTable = createCellTable();
		cellTable.setWidth("100%");
		cellTableContainer.setWidget(cellTable);
		cellTable.setPageSize(PAGE_SIZE);
		
		singleSelectionModel = new SingleSelectionModel<T>();
		cellTable.setSelectionModel(singleSelectionModel);
		
		SimplePanel pagerContainer = new SimplePanel();
		pagerContainer.setStyleName("paging");
		listPanel.add(pagerContainer);
		
		simplePager = new SimplePager();
		simplePager.setDisplay(cellTable);
		simplePager.setStyleName("pager");
		pagerContainer.setWidget(simplePager);
		
		cellTable.getColumn(0).setCellStyleNames("firstColumn");
		cellTable.getHeader(0).setHeaderStyleNames("firstColumn");
	}
	
	@Override
	public FlowPanel asWidget() {
		return this;
	}
	
	@Override
	public ToolBarView getToolBarView() {
		return toolBarView;
	}

	@Override
	public SimplePager getPager() {
		return this.simplePager;
	}

	@Override
	public CellTable<T> getCellTable() {
		return this.cellTable;
	}
	
	@Override
	public AsyncDataProvider<T> getDataProvider() {
		return this.dataProvider;
	}

	@Override
	public SingleSelectionModel<T> getSingleSelectionModel() {
		return this.singleSelectionModel;
	}

	@Override
	public void setDataProvider(AsyncDataProvider<T> provider) {
		this.dataProvider = provider;
		this.dataProvider.addDataDisplay(cellTable);
	}

	protected TextColumn<T> createIdColumn() {
		TextColumn<T> column = new TextColumn<T>() {
			@Override
			public String getValue(T object) {
				return getEntityId(object);
			}
		};
		return column;
	}
	
	protected Column<T, String> createNameColumn() {
		Column<T, String> column =  new TextColumn<T>() {
			@Override
			public String getValue(T object) {
				return getEntityName(object);
			}
		};
		return column;
	}
	
	protected Column<T, String> createEditColumn() {
		final Column<T, String> column = new Column<T, String>(new FontawesomeButtonCell()) {
		    @Override
		    public String getValue(T object) {
		        return "<i class=\"material-icons\">keyboard_arrow_right</i>";
		    }
		};
		return column;
	}
	
	protected String getEntityId(T object) {
		if (object == null) {
			return "";
		}
		return object.getId() + "";
	}

	protected String getEntityName(T object) {
		if (object == null) {
			return "";
		}
		return object.getName();
	}
	
	@Override
	public void showEditForm() {
		listPanel.setStyleName("list " + " collapsed");
	}
	
	@Override
	public void hideEditForm() {
		listPanel.setStyleName("list " +" expanded");
	}
}
