package net.ionoff.center.client.project;

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
import net.ionoff.center.client.service.ProjectService;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ProjectDto;

public class ProjectTablePresenter extends SystemTablePresenter<ProjectDto>{

	public interface Display extends ITableView<ProjectDto> {
		Column<ProjectDto, String> getNameColumn();
		Column<ProjectDto, String> getAddressColumn();
		net.ionoff.center.client.project.ProjectEditPresenter.Display getProjectEditView();
	}

	private final Display view;
	private ProjectEditPresenter projectEditPresenter; 

	public ProjectTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}

	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<ProjectDto, String>() {
			@Override
			public void update(int index, ProjectDto object, String value) {
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
	}

	@Override
	protected void add() {
		final List<ProjectDto> projectDtos = new ArrayList<ProjectDto>();
		final ProjectDto newProjectDto = newProjectDto();
		projectDtos.add(newProjectDto);
		projectDtos.addAll(getDisplayingDtos());
		showEntities(projectDtos, newProjectDto.getId(), 0);
		showEditForm();
		getProjectEditPresenter().setEntityDto(newProjectDto);
	}

	private ProjectDto newProjectDto() {
		final ProjectDto newProjectDto = new ProjectDto();
		newProjectDto.setId(BaseDto.DEFAULT_ID);
		newProjectDto.setName("*" + AdminLocale.getAdminConst().project());
		newProjectDto.setAddress(AdminLocale.getAdminConst().address());
		return newProjectDto;
	}

	@Override
	protected AsyncDataProvider<ProjectDto> newAsyncDataProvider() {
		final AsyncDataProvider<ProjectDto> provider = new AsyncDataProvider<ProjectDto>() {
			@Override
			protected void onRangeChanged(HasData<ProjectDto> hasData) {
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
	protected boolean isSameId(ProjectDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return ProjectDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		return "name";
	}

	@Override
	protected void loadByRange(final Long selectedId, final boolean onSort, 
			final int startIndex) {

		getRpcService().searchByCriteria(buildSearchCriteria(startIndex), 
				new MethodCallback<List<ProjectDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ProjectDto> result) {
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
	protected ProjectService getRpcService() {
		return rpcProvider.getProjectService();
	}

	@Override
	protected String getSortByField(int columnIndex) {
		return BaseDto.NAME;
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	private void showEditForm() {
		view.showEditForm();
		
	}
	
	@Override
	protected void setSelectedObject(ProjectDto selectedDto) {
		getProjectEditPresenter().setEntityDto(selectedDto);
	} 
	
	public ProjectEditPresenter getProjectEditPresenter() {
		if (projectEditPresenter == null) {
			projectEditPresenter = new ProjectEditPresenter(rpcProvider, eventBus, view.getProjectEditView(), this);
			projectEditPresenter.go();
		}
		return projectEditPresenter;
	}
}
