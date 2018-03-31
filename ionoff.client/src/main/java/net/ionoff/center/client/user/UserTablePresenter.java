package net.ionoff.center.client.user;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.UserDto;

public class UserTablePresenter extends SystemUsersPresenter {
	
	public UserTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
	}
	

	@Override
	protected void loadByRange(Long selectedId, boolean onSort, int startIndex) {
		getRpcService().searchByCriteria(buildSearchCriteria(startIndex), new MethodCallback<List<UserDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<UserDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				if (onSort) {
					display.getPager().setPage(0);
				}
				showEntities(result, selectedId, startIndex);
			}
		});
	}

	@Override
	protected void loadData(Long selectedId, boolean onSort, int startIndex) {
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
	protected Long getProjectId() {
		return AppToken.getProjectIdLong();
	}
}
