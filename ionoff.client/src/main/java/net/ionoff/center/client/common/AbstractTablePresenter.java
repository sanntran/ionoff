package net.ionoff.center.client.common;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;

public abstract class AbstractTablePresenter<T extends BaseDto> extends SystemTablePresenter<T> {


	public AbstractTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, ITableView<T> view) {
		super(rpcProvider, eventBus, view);
	}

	@Override
	protected void loadByRange(final Long selectedId, final boolean onSort, 
			final int startIndex) {

		getRpcService().searchByCriteria(buildSearchCriteria(startIndex), 
				new MethodCallback<List<T>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<T> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				if (onSort) {
					display.getPager().setPage(0);
				}
				showEntities(result, selectedId, startIndex);
			}
		});
	}

	@Override
	protected void loadData(final Long selectedId, final boolean onSort, final int startIndex) {

		getRpcService().countByCriteria(buildCountCriteria(), new MethodCallback<Long>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, Long result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				display.getDataProvider().updateRowCount(result.intValue(), true);
				loadByRange(selectedId, onSort, startIndex);
			}
		});
	}

	@Override
	protected abstract EntityService<T> getRpcService();
}
