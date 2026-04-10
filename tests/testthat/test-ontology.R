skip_if_api_down <- function(
        url = 'https://www.ebi.ac.uk/ols4/api/v2/stats',
        method = "GET",
        timeout = 2,
        ok_status = 200:399,    # Status codes considered "healthy"
        allow_status = NULL     # Optional override (e.g. c(429, 403))
) {
        # Build the HTTP request
        req <- httr2::request(url)

        # set HTTP method (HEAD by default)
        req <- httr2::req_method(req, method)

        # Apply timeout
        req <- httr2::req_timeout(req, timeout)

        # Perform the request
        resp <- httr2::req_perform(req)

        # get status code
        status <- httr2::resp_status(resp)

        # handle allowed status
        if (!is.null(allow_status) && status %in% allow_status) {
            return(invisible(TRUE))  # Do nothing; test continues
        }

        # check allowed range
        if (!(status %in% ok_status)) {

            # Skip with message
            testthat::skip(sprintf(
                "API endpoint not healthy [%s]: HTTP %d",
                url, status
            ))
        }

        # success
        invisible(TRUE)
    }


# test ontology_term object
test_that('ontology_term creates object with OLS API data', {
    skip_if_offline()
    skip_if_api_down()

    O = ontology_term('STATO:0000046')
    expect_true(is(O, 'ontology_term'))
    expect_true(O$id == 'STATO:0000046')
    expect_true(length(O$label) > 0)
    expect_true(length(O$description) > 0)
    expect_true(length(O$iri) > 0)
})

test_that('ontology_term validates ID format', {
    # Invalid ID format should error (no API call)
    expect_error(
        ontology_term('invalid-id'),
        'Invalid ontology ID format'
    )

    expect_error(
        ontology_term('STATO'),
        'Invalid ontology ID format'
    )
})

test_that('ontology_term handles API unavailability gracefully', {
    skip_if_offline()
    skip_if_api_down()

    # Invalid obo_id may not be found, but object should still be created
    expect_warning(
        O <- ontology_term('INVALID:9999999'),
        'Could not fetch ontology term'
    )
    expect_true(is(O, 'ontology_term'))
    expect_true(O$id == 'INVALID:9999999')
})

test_that('ontology_term extracts ontology prefix from ID', {
    skip_if_offline()
    skip_if_api_down()

    O = ontology_term('STATO:0000046')
    expect_true(O$ontology == 'stato')

    O = ontology_term('OBI:0200051')
    expect_true(O$ontology == 'obi')
})

# test ontology_list object
test_that('ontology_list from character IDs', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046', 'OBI:0200051')
    expect_true(is(OL, 'ontology_list'))
    expect_equal(length(OL), 2)
    expect_true(OL[[1]]$id == 'STATO:0000046')
    expect_true(OL[[2]]$id == 'OBI:0200051')
})

test_that('ontology_list empty list', {
    OL = ontology_list()
    expect_true(is(OL, 'ontology_list'))
    expect_equal(length(OL), 0)
})

test_that('ontology_list subsetting with [', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046', 'OBI:0200051')

    # Subset single element returns ontology_list
    OL_subset = OL[1]
    expect_true(is(OL_subset, 'ontology_list'))
    expect_equal(length(OL_subset), 1)

    # Subset multiple elements
    OL_subset = OL[1:2]
    expect_true(is(OL_subset, 'ontology_list'))
    expect_equal(length(OL_subset), 2)
})

test_that('ontology_list element extraction with [[', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046', 'OBI:0200051')

    # Extract single element returns ontology_term
    O = OL[[1]]
    expect_true(is(O, 'ontology_term'))
    expect_true(O$id == 'STATO:0000046')
})

test_that('ontology_list assignment with character ID', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046', 'OBI:0200051')

    # Assign character ID
    OL[1] = 'OBI:0200051'
    expect_true(OL[[1]]$id == 'OBI:0200051')
})

test_that('ontology_list validates assignment input', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046')

    # Invalid input should error
    expect_error(
        {OL[1] = struct_class()},
        'value must be an ontology_term'
    )

    expect_error(
        {OL[1] = 123},
        'value must be an ontology_term'
    )
})


test_that('ontology_list show method', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046')

    expect_output(show(OL[[1]]), regexp = 'STATO:0000046')
})

test_that('ontology_list invalid input', {
    expect_error(
        ontology_list(struct_class()),
        'All terms must be character conforming to the ontology id format: PREFIX:DIGITS e.g. STATO:0000046'
    )

    expect_error(
        ontology_list(123),
        'All terms must be character conforming to the ontology id format: PREFIX:DIGITS e.g. STATO:0000046'
    )
})

test_that('ontology_list slot access', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046')

    # Access terms slot
    terms = OL$terms
    expect_true(is.list(terms))
    expect_equal(length(terms), 1)
})

test_that('ontology_list invalid slot access errors', {
    skip_if_offline()
    skip_if_api_down()

    OL = ontology_list('STATO:0000046')

    expect_error(OL$cake)
})

test_that('ontology method on struct objects', {
    M = example_model(ontology = 'STATO:0000046')
    expect_equal(M$ontology, 'STATO:0000046')
})

test_that("ontology_set_online(FALSE) skips OLS without warning", {
    op <- options(struct.ontology.online = FALSE)
    on.exit(options(op), add = TRUE)
    expect_silent(
        O <- ontology_term("STATO:0000046")
    )
    expect_true(is(O, "ontology_term"))
    expect_identical(O$id, "STATO:0000046")
    expect_identical(O$ontology, "stato")
    expect_identical(O$label, character())
    expect_identical(O$description, character())
    expect_identical(O$iri, character())
})

test_that("ontology_set_online sets option and returns invisibly", {
    op <- options(struct.ontology.online = TRUE)
    on.exit(options(op), add = TRUE)
    expect_invisible(ontology_set_online(FALSE))
    expect_false(isTRUE(getOption("struct.ontology.online", TRUE)))
    expect_invisible(ontology_set_online(TRUE))
    expect_true(isTRUE(getOption("struct.ontology.online", TRUE)))
})
