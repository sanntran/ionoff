package net.ionoff.center.client.device;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.cell.client.FieldUpdater;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.datepicker.client.CalendarUtil;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;

import gwt.material.design.client.ui.MaterialLink;
import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.service.SensorDataService;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.WeighScaleDto;

public class ScaleDataTablePresenter extends AbstractTablePresenter<SensorDataDto> {


    public interface Display extends ITableView<SensorDataDto> {
        Column<SensorDataDto, String> getNameColumn();
        ScaleDataEditPresenter.Display getScaleDataEditView();
		MaterialLink getLinkDownloadPdfFile();
		MaterialLink getLinkDownloadExcelFile();
		MaterialLink getLinkDownloadWordFile();
    }

    private final Display view;
    private WeighScaleDto scaleDto;
    private ScaleDataEditPresenter dataEditPresenter;

    public ScaleDataTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
                                   WeighScaleDto scaleDto, Display view) {
        super(rpcProvider, eventBus, view);
        setScale(scaleDto);
        this.view = view;
    }

    @Override
    public void bind() {
        super.bind();
        view.getEditColumn().setFieldUpdater(new FieldUpdater<SensorDataDto, String>() {
            @Override
            public void update(int index, SensorDataDto object, String value) {
                showEditForm();
            }
        });
        view.getLinkDownloadPdfFile().addClickHandler(event -> downloadReportFile("pdf"));
        view.getLinkDownloadExcelFile().addClickHandler(event -> downloadReportFile("xlsx"));
        view.getLinkDownloadWordFile().addClickHandler(event -> downloadReportFile("docx"));
    }

    private void downloadReportFile(String fileType) {
    	eventBus.fireEvent(new ShowMessageEvent(ProjectLocale.getProjectMessages().generatingReport(), 
    			ShowMessageEvent.NORMAL));
		QueryCriteriaDto criteria = buildSearchAllCriteria();
		rpcProvider.getSensorDataService().exportToReportFile(criteria, fileType,
				new MethodCallback<MessageDto>() {
					@Override
					public void onFailure(Method method, Throwable exception) {
						eventBus.fireEvent(new ShowMessageEvent(ProjectLocale.getProjectMessages()
								.errorGeneratingReport(), ShowMessageEvent.ERROR));
						ClientUtil.handleRpcFailure(method, exception, eventBus);
					}
					@Override
					public void onSuccess(Method method, MessageDto result) {
						eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
						eventBus.fireEvent(new ShowMessageEvent(ProjectLocale.getProjectMessages()
								.finishGeneratingReport(), ShowMessageEvent.SUCCESS));
						String url = ClientUtil.getBaseUrl() + "/" + result.getMessage();
						if (fileType.equals("pdf")) {
							Window.open(url, "_blank", null);
						}
						else {
							Window.open(url, "_self", null);
						}
					}
				});
	}

	private QueryCriteriaDto buildSearchAllCriteria() {
		QueryCriteriaDto criteriaDto = super.buildSearchCriteria(0);
		criteriaDto.setFromIndex(0);
		criteriaDto.setMaxResults(0);
        criteriaDto.setDeviceId(scaleDto.getId());
        return criteriaDto;
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
        // does nothing
    }

    public void setScale(WeighScaleDto scaleDto) {
        this.scaleDto = scaleDto;
        display.getToolBarView().getLblTitle().setText(scaleDto.getName());
    }

    @Override
    protected boolean validateBeforeSaving() {
        return true;
    }

    @Override
    protected void add() {
        // does nothing
    }

    @Override
    protected AsyncDataProvider<SensorDataDto> newAsyncDataProvider() {
        final AsyncDataProvider<SensorDataDto> provider = new AsyncDataProvider<SensorDataDto>() {
            @Override
            protected void onRangeChanged(HasData<SensorDataDto> hasData) {
                final int start = hasData.getVisibleRange().getStart();
                // int length = hasData.getVisibleRange().getLength();
                loadData(null, false, start);
            }
        };
        return provider;
    }

    @Override
    protected void loadByRange(final Long selectedId, final boolean onSort,
                               final int startIndex) {

        getRpcService().calculateSumByDay(buildSearchCriteria(startIndex),
                new MethodCallback<List<SensorDataDto>>() {
                    @Override
                    public void onFailure(Method method, Throwable exception) {
                        ClientUtil.handleRpcFailure(method, exception, eventBus);
                    }
                    @Override
                    public void onSuccess(Method method, List<SensorDataDto> result) {
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
        int days = 7;
        if (display.getToolBarView().getDateBoxFrom().getValue() != null
            && display.getToolBarView().getDateBoxTo().getValue() != null) {
            days = CalendarUtil.getDaysBetween(
                    display.getToolBarView().getDateBoxFrom().getValue(),
                    display.getToolBarView().getDateBoxTo().getValue());
        }
        display.getDataProvider().updateRowCount(days, true);
        loadByRange(selectedId, onSort, startIndex);
    }

    @Override
    protected void fillListBoxSearchBy() {
        display.getToolBarView().getLisBoxSearchBy().addItem(AdminLocale.getAdminConst().time());
    }

    @Override
    protected boolean isSameId(SensorDataDto entity, Long selectedId) {
        return entity.getId() == selectedId;
    }

    @Override
    protected String getClazz() {
        return SensorDataDto.class + "";
    }

    @Override
    protected String getSearchBy() {
        return "time";
    }

    @Override
    protected SensorDataService getRpcService() {
        return rpcProvider.getSensorDataService();
    }

    @Override
    protected String getSortByField(int columnIndex) {
        return "time";
    }

    public ScaleDataEditPresenter getDataEditPresenter() {
        if (dataEditPresenter == null) {
            dataEditPresenter = new ScaleDataEditPresenter(rpcProvider, eventBus,
                    view.getScaleDataEditView(), this);
            dataEditPresenter.go();
        }
        return dataEditPresenter;
    }

    public void hideEditForm() {
        view.hideEditForm();
    }

    @Override
    protected void setSelectedObject(SensorDataDto selectedDto) {
        getDataEditPresenter().setWeighScale(scaleDto);
        getDataEditPresenter().setEntityDto(selectedDto);
    }

    private void showEditForm() {
        view.showEditForm();
    }

    @Override
    protected String getKeySearch() {
        return display.getToolBarView().getDateBoxFromText() + " 00:00:00"
                + "-" + display.getToolBarView().getDateBoxToText() + " 23:59:59";
    }

    @Override
    protected QueryCriteriaDto buildSearchCriteria(int startIndex) {
        QueryCriteriaDto criteriaDto = super.buildSearchCriteria(startIndex);
        criteriaDto.setDeviceId(scaleDto.getId());
        return criteriaDto;
    }

    @Override
    protected QueryCriteriaDto buildCountCriteria() {
        QueryCriteriaDto criteriaDto = super.buildCountCriteria();
        criteriaDto.setDeviceId(scaleDto.getId());
        return criteriaDto;
    }
}