#' Fit an SSNIP test (note: invoked by fit_ssnip())
#'
#' @param df Data frame
#' @param target The target hospital for SSNIP test.
#' @param A Adjacency matrix
#'
#' @return
#' @export

ssnip_ <- function(df, A, target) {
    Var1 <- Var2 <- hospital <- test <- delta_wtp <- wtp_delta <- market <- NULL
    m <- df %>% tidyr::gather(hospital,market) %>% tibble::deframe()
    mm <- m[which(m %in% m[target])]
    expand.grid(names(mm),names(mm)) %>%
        dplyr::filter(grepl(target,Var1)) %>%
        dplyr::filter(Var1 != Var2) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(test = purrr::map2(Var1,Var2,~({
            A_ <-
                A[,c(names(mm))] %>%
                as.matrix()
            colnames(A_) <- names(mm)
            wtp_j <-
                A_ %>%
                calculate_wtp()

            merged <- c(.x,.y)
            A_same <- A_[,-which(colnames(A_) %in% merged)]
            if (is.null(dim(A_same))) {
                A_same <- A_same %>% as.matrix()
                colnames(A_same) <- setdiff(colnames(A_),merged)
            }
            A_new <- apply(A_[,which(colnames(A_) %in% merged)],1,sum)
            A_merged <- cbind(A_same,A_new)
            colnames(A_merged) <- c(colnames(A_same),paste0(merged,collapse ="_"))

            wtp_merged <-
                A_merged %>%
                calculate_wtp()
            diff <- wtp_merged[paste0(merged,collapse ="_")] - sum(wtp_j[merged])
            names(diff) = "delta_wtp"
            wtp <- c(wtp_j[merged],wtp_merged[paste0(merged,collapse = "_")],diff)
            data.frame(wtp) %>%
                tibble::rownames_to_column(var = "firm") %>%
                tidyr::spread(firm,wtp) %>%
                dplyr::select_at(vars(.x,.y, everything())) %>%
                dplyr::as_tibble()
        }))) %>%
        tidyr::unnest(cols = test) %>%
        dplyr::select(source = Var1, target = Var2, wtp_delta = delta_wtp) %>%
        dplyr::arrange(dplyr::desc(wtp_delta))
}
