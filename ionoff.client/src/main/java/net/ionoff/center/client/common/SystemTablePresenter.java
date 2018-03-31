package net.ionoff.center.client.common;

import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.ColumnSortEvent;
import com.google.gwt.user.cellview.client.ColumnSortEvent.AsyncHandler;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.SelectionChangeEvent;
import com.google.gwt.view.client.SelectionChangeEvent.Handler;

import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

public abstract class SystemTablePresenter<T extends BaseDto> extends AbstractPresenter {

	protected ITableView<T> display;
	protected IRpcServiceProvider rpcProvider;

	private T unsavedDto;
	protected String sortFieldName;
	protected boolean isSortAscending;
	

	public SystemTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, ITableView<T> view) {
		super(eventBus);
		this.rpcProvider = rpcProvider;
		this.display = view;
		isSortAscending = true;
		sortFieldName = BaseDto.NAME;
	}

	protected boolean validateBeforeSaving() {
		if (getUnsavedDto() == null) {
			return false;
		}
		return validateInputStringValue(AdminLocale.getAdminConst().name(), getUnsavedDto().getName());
	}

	protected abstract void save();

	protected abstract void add();
	
	protected abstract void setSelectedObject(T object);

	protected abstract AsyncDataProvider<T> newAsyncDataProvider();

	protected abstract void fillListBoxSearchBy();

	protected abstract boolean isSameId(T entity, Long selectedId);

	protected abstract String getClazz();
	
	protected abstract String getSortByField(int columnIndex);

	protected abstract String getSearchBy();
	
	protected abstract EntityService<T> getRpcService();

	protected abstract void loadByRange(final Long selectedId, final boolean onSort, 
			final int startIndex);

	protected abstract void loadData(final Long selectedId, final boolean onSort, 
			final int startIndex);

	protected void bind() {

		fillListBoxSearchBy();

		display.getToolBarView().getTextBoxKeyWord().addKeyUpHandler(new KeyUpHandler() {
			@Override
			public void onKeyUp(KeyUpEvent event) {
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
					search();
				}
			}
		});
		display.getToolBarView().getBtnSearch().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (display.getToolBarView().getSearchPanel().isVisible()) {
					search();
				}
				else {
					display.getToolBarView().getSearchPanel().setVisible(true);
				}
			}
		});
		//
		display.getToolBarView().getBtnRefresh().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});
		//
		display.getToolBarView().getBtnAdd().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (hasNewEntity()) {
					eventBus.fireEvent(new ShowMessageEvent(
							AdminLocale.getAdminMessages().newRowUnsaved(BaseDto.DEFAULT_ID), ShowMessageEvent.ERROR));
				}
				else {
					add();
				}
			}
		});
		//
		display.getToolBarView().getBtnRemove().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				delete();
			}
		});
		// /////////////////////////////////////////////////////////

		display.getSingleSelectionModel().addSelectionChangeHandler(new Handler() {
			@Override
			public void onSelectionChange(final SelectionChangeEvent event) {
				final T selectedDto = display
						.getSingleSelectionModel().getSelectedObject();
				setSelectedObject(selectedDto);
			}
		});

		final AsyncDataProvider<T> provider = newAsyncDataProvider();
		display.setDataProvider(provider);

		final AsyncHandler columnSortHandler = new AsyncHandler(display.getCellTable()) {
			@SuppressWarnings("unchecked")
			@Override
			public void onColumnSort(ColumnSortEvent event) {
				int sortIndex = display.getCellTable().getColumnIndex((Column<T, ?>) event.getColumn());
				// Default sort is already by ascending
				sortFieldName = getColumnFieldName(sortIndex); 
				isSortAscending = !event.isSortAscending();
				
				loadByRange(null, false, 0);
			}
		};
		display.getCellTable().addColumnSortHandler(columnSortHandler);
	}
	
	protected boolean hasNewEntity() {
		for (final BaseDto entity : getDisplayingDtos()) {
			if (entity.getId() == BaseDto.DEFAULT_ID) {
				return true;
			}
		}
		return false;
	}

	public void onSavedSucess(T dto) {
		eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
				ShowMessageEvent.SUCCESS));
		final int startIndex = display.getCellTable().getVisibleRange().getStart();
		loadData(dto.getId(), false, startIndex);
	}

	protected void delete() {
		final T dto = getSelectedObject();
		if (dto == null) {
			return;
		}
		if (dto.getId() == BaseDto.DEFAULT_ID) {
			deleteTemporaryDto(dto);
			refresh();
			return;
		}
		rpcDelete(dto.getId());
	}

	private void rpcDelete(long entityId) {
		getRpcService().delete(entityId, new MethodCallback<MessageDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, MessageDto result) {
				eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().deleteSuccess(),
						ShowMessageEvent.SUCCESS));
				refresh();
			}
		});
	}

	private void deleteTemporaryDto(T dto) {
		final List<T> dtos = new ArrayList<T>();
		dtos.addAll(getDisplayingDtos());
		if (dtos.contains(dto)) {
			dtos.remove(dto);
		}
	}

	public void refresh() {
		loadData(null, false, display.getCellTable().getVisibleRange().getStart());
	}

	protected void search() {
		loadData(null, false, 0);
	}

	protected void showEntities(List<T> dtos, Long selectedId, int startIndex) {
		setUnsavedDto(null);
		display.getDataProvider().updateRowData(startIndex, dtos);
		final int index = getIndex(dtos, selectedId);
		if (dtos != null && dtos.size() > 0) {
			display.getSingleSelectionModel().setSelected(dtos.get(index), true);
		}
	}

	protected List<T> getDisplayingDtos() {
		return display.getCellTable().getVisibleItems();
	}

	private int getIndex(List<T> dtos, Long selectedId) {
		if (selectedId == null) {
			return 0;
		}
		int n = 0;
		if (dtos != null) {
			n = dtos.size();
		}
		for (int i = 0; i < n; i++) {
			if (dtos.get(i) != null && isSameId(dtos.get(i), selectedId))
				return i;
		}
		return 0;
	}

	protected String getKeySearch() {
		return display.getToolBarView().getTextBoxKeyWord().getText();
	}


	protected boolean validateInputNumberValue(String field, String value) {
		if (value == null || value.trim().isEmpty()) {
			final String message = AdminLocale.getAdminMessages().emptyInputValue(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		if (!ClientUtil.isIntNumber(value)) {
			final String message = AdminLocale.getAdminMessages().invalidNumberValue(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		return true;
	}

	protected boolean validateInputStringValue(String field, String value) {
		if (value == null || value.trim().isEmpty()) {
			final String message = AdminLocale.getAdminMessages().emptyInputValue(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		if (value.length() > 250) {
			final String message = AdminLocale.getAdminMessages().overMaximunLength(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		if (value.contains("#")) {
			final String message = AdminLocale.getAdminMessages().containsSpecialCharacters(field);
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		return true;
	}

	protected T getSelectedObject() {
		return display.getSingleSelectionModel().getSelectedObject();
	}

	public T getUnsavedDto() {
		return unsavedDto;
	}

	public void setUnsavedDto(T unsavedDto) {
		this.unsavedDto = unsavedDto;
	}

	protected int getDisplayRowIndex(int index) {
		return index % ITableView.PAGE_SIZE;
	}
	
	private String getColumnFieldName(int columnIndex) {
		if (columnIndex == 0) {
			return BaseDto.ID;
		}
		else if (columnIndex == 1) {
			return BaseDto.NAME;
		}
		else {
			return getSortByField(columnIndex);
		}
	}

	protected QueryCriteriaDto buildCountCriteria() {
		QueryCriteriaDto criteriaDto = new QueryCriteriaDto();
		criteriaDto.setProjectId(getProjectId());
		criteriaDto.setSearchKey(getKeySearch());
		criteriaDto.setSearchField(getSearchBy());
		return criteriaDto;
	}
	
	protected QueryCriteriaDto buildSearchCriteria(int startIndex) {
		QueryCriteriaDto criteriaDto = new QueryCriteriaDto();
		criteriaDto.setProjectId(getProjectId());
		criteriaDto.setSearchKey(getKeySearch());
		criteriaDto.setSearchField(getSearchBy());
		criteriaDto.setFromIndex(startIndex);
		criteriaDto.setMaxResults(display.getCellTable().getPageSize());
		criteriaDto.setSortBy(sortFieldName);
		criteriaDto.setIsAscending(isSortAscending);
		return criteriaDto;
	}
	
	protected Long getProjectId() {
		if (AppToken.hasTokenItem(AppToken.PROJECT)) {
			return AppToken.getProjectIdLong();
		}
		return null;
	}
}
