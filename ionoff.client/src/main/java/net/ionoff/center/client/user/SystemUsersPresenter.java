package net.ionoff.center.client.user;

import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.cell.client.FieldUpdater;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;

import net.ionoff.center.client.common.ITableView;
import net.ionoff.center.client.common.SystemTablePresenter;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.service.UserService;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.UserGroupDto;
import net.ionoff.center.shared.dto.UserDto;

public class SystemUsersPresenter extends SystemTablePresenter<UserDto> {
	
	public interface Display extends ITableView<UserDto> {
		
		Column<UserDto, String> getNameColumn();

		Column<UserDto, String> getFullNameColumn();

		UserEditPresenter.Display getUserEditView();
	}
	
	private final Display view;
	private UserEditPresenter userEditPresenter;
	
	public SystemUsersPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<UserDto, String>() {
			@Override
			public void update(int index, UserDto object, String value) {
				showEditForm();
			}
		});
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asWidget());
	}

	@Override
	protected void save() {
		rpcProvider.getUserService().save(getUnsavedDto().getId(), getUnsavedDto(), 
				new MethodCallback<UserDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, UserDto result) {
				onSavedSucess(result);
			}
		});
	}

	@Override
	protected void add() {
		List<UserDto> userDtos = new ArrayList<UserDto>();
		UserDto newUserDto = newUserDto();
		userDtos.add(newUserDto);
		userDtos.addAll(getDisplayingDtos());
		showEntities(userDtos, newUserDto.getId(), 0);
		showEditForm();
		setSelectedObject(newUserDto);
	}

	private UserDto newUserDto() {
		UserDto newUserDto = new UserDto();
		newUserDto.setId(BaseDto.DEFAULT_ID);
		newUserDto.setName("*" + AdminLocale.getAdminConst().user());
		newUserDto.setFullName(AdminLocale.getAdminConst().fullName());
		newUserDto.setPassword("123456");
		newUserDto.setPhoneNo(AdminLocale.getAdminConst().phoneNumber());
		newUserDto.setEmail(AdminLocale.getAdminConst().email());
		newUserDto.setGroupName(UserGroupDto.PROJECT_USER);
		return newUserDto;
	}

	@Override
	protected AsyncDataProvider<UserDto> newAsyncDataProvider() {
		final AsyncDataProvider<UserDto> provider = new AsyncDataProvider<UserDto>() {
			@Override
			protected void onRangeChanged(HasData<UserDto> hasData) {
				final int start = hasData.getVisibleRange().getStart();
				// int length = hasData.getVisibleRange().getLength();
				loadData(null, false, start);
			}
		};
		return provider;
	}

	@Override
	protected void fillListBoxSearchBy() {
		display.getToolBarView().getLisBoxSearchBy().addItem(AdminLocale.getAdminConst().name());
	}

	@Override
	protected boolean isSameId(UserDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return UserDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		return "name";
	}

	@Override
	protected UserService getRpcService() {
		return rpcProvider.getUserService();
	}
	
	@Override
	protected String getSortByField(int columnIndex) {
		return BaseDto.NAME;
	}
	
	public UserEditPresenter getUserEditPresenter() {
		if (userEditPresenter == null) {
			userEditPresenter = new UserEditPresenter(rpcProvider, eventBus, view.getUserEditView(), this);
			userEditPresenter.go();
		}
		return userEditPresenter;
	}

	public void hideEditForm() {
		view.hideEditForm();
	}

	private void showEditForm() {
		view.showEditForm();
	}
	
	@Override
	protected void setSelectedObject(UserDto selectedDto) {
		getUserEditPresenter().setEntityDto(selectedDto);
	}

	@Override
	protected void loadByRange(Long selectedId, boolean onSort, int startIndex) {
		rpcProvider.getUserService().searchByCriteria(buildSearchCriteria(startIndex)
				, new MethodCallback<List<UserDto>>() {
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
		
		rpcProvider.getUserService().countByCriteria(buildCountCriteria(), new MethodCallback<Long>() {
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
}
